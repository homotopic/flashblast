{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

import           Composite.Record
import qualified Data.Attoparsec.Text     as A
import qualified Dhall                    as D
import           Network.HTTP.Simple
import           Path
import           Path.Dhall               ()
import           Path.Utils
import           Polysemy
import           Polysemy.Error           as P
import           Polysemy.Input
import           Polysemy.KVStore
import           Polysemy.Trace
import           Polysemy.Video

import           Colog.Polysemy
import           FlashBlast.AnkiDB
import           FlashBlast.ClozeParse
import           FlashBlast.Config
import           FlashBlast.FBFileSystem
import           FlashBlast.Conventions
import           FlashBlast.ForvoClient
import           FlashBlast.JSONFileStore
import           FlashBlast.YouTubeDL
import           Polysemy.State
import           RIO                      hiding (Reader, ask, asks, log, many,
                                           runReader, trace)
import           RIO.List
import qualified RIO.Map                  as Map
import qualified RIO.Text                 as T
import qualified Text.Subtitles.SRT       as SR
import qualified Turtle                   as S


fromTime :: SR.Time -> Time
fromTime (SR.Time h m s f) = Time h m s f

fromRange :: SR.Range -> Range
fromRange (SR.Range f t) = Range (fromTime f) (fromTime t)

interpretVideoSource :: Members '[Input ResourceDirs, YouTubeDL] m => VideoSource -> Sem m (Path Rel File)
interpretVideoSource = \case
  YouTubeDL (YDLInfo x y f) -> do
    ResourceDirs{..} <- input @ResourceDirs
    youTubeDL' x (video </> y) f
    return (video </> y)
  LocalVideo x -> do
    ResourceDirs{..} <- input @ResourceDirs
    return (video </> x)

runExcerptSpecIO :: Members '[Error SubtitleParseException
                       , FBFileSystem
                       , Input ExportDirs
                       , Input ResourceDirs
                       , YouTubeDL
                       , ClipProcess] m
                       => ExcerptSpec -> Sem m [RExcerptNote]
runExcerptSpecIO ExcerptSpec {..} = do
  ResourceDirs{..} <- input @ResourceDirs
  ExportDirs{..} <- input @ExportDirs
  t <- interpretVideoSource source
  s' <- either (throw . SubtitleParseException) return $ A.parseOnly SR.parseSRT subs
  let cs = map (clipf  . T.pack . show . SR.index) s'
  let es = map (audiof . T.pack . show . SR.index) s'
  let fs = map (framef . T.pack . show . SR.index) s'
  cs' <- filterM (fmap not . doesFileExist . (clips </>)) cs
  es' <- filterM (fmap not . doesFileExist . (audio </>)) es
  h <- createTempDirectory
  createDirectory clips
  createDirectory audio
  createDirectory images
  unless (null cs') $ do
    extractClips t $ zip (fromRange . SR.range <$> s') (h </$> cs')
    forM_ cs' $ \x -> copyFile (h </> x) (clips </> x)
  unless (null es') $ do
    extractAudio t $ zip (fromRange . SR.range <$> s') (h </$> es)
    forM_ es' $ \x -> copyFile (h </> x) (audio </> x)
  removeDirectory h
  forM (zip4 s' cs es fs) $ \(l, c, e, f) -> do
    whenM (fmap not . doesFileExist $ images </> f) $
      extractFrames (clips </> c) [(Time 0 0 0 0, images </> f)]
    return $ val @"front" (fst . genClozePhrase . SR.dialog $ l)
          :& val @"extra" f
          :& val @"back"  e
          :& RNil

newtype SubtitleParseException = SubtitleParseException String
  deriving (Eq, Show, Generic)

instance Exception SubtitleParseException

type FSPKVStore = KVStore (Locale, Text) ForvoStandardPronunciationResponseBody

downloadMP3For :: Members [FSPKVStore, ForvoClient, Trace] r => Locale -> Text -> Sem r (Maybe ByteString)
downloadMP3For l t = do
  a <- lookupKV @(Locale, Text) @ForvoStandardPronunciationResponseBody (l, t)
  case a of
    Just x -> do
      trace $ "Response for " <> show (l, t) <> " found in cache."
      p x
    Nothing -> do
      x <- standardPronunciation l t
      updateKV @(Locale, Text) @ForvoStandardPronunciationResponseBody (l,t) $ Just x
      p x
  where
    p :: Members '[ForvoClient, Trace] r => ForvoStandardPronunciationResponseBody -> Sem r (Maybe ByteString)
    p x = case items x of
      []      -> return Nothing
      (x':xs) -> Just <$> mP3For x'

getForvo :: Members '[Trace, FBFileSystem, FSPKVStore, ForvoClient] r => Locale -> Text -> Path Rel File -> Sem r ()
getForvo l t f = do
  z <- doesFileExist f
  case z of
    True  -> trace $ show f <> " already exists in filesystem."
    False -> do
      trace $ show f <> " not found in filesystem."
      x <- downloadMP3For l t
      case x of
        Just x' -> do
          createDirectory (parent f)
          writeFileBS f x'
        Nothing -> return ()

runMultiClozeSpecIO :: Members '[ RemoteHttpRequest
                                , Trace
                                , Input ResourceDirs
                                , Error SomeException
                                , FBFileSystem
                                , Error JSONException
                                , FSPKVStore
                                , State (Maybe ForvoSpec)] m
                    => (Text -> Path Rel File)
                    -> MultiClozeSpec
                    -> Sem m [RForvoNote]
runMultiClozeSpecIO f (MultiClozeSpec p is) = do
    ResourceDirs{..} <- input @ResourceDirs
    forM p \a -> do
      let (bs, cs) = genClozePhrase a
      s <- get @(Maybe ForvoSpec)
      forM_ s $ \(ForvoSpec l api) ->
        forM cs $ \t -> do
          z <- P.try @JSONException $
            runInputConst @ForvoAPIKey api . interpretForvoClient $ getForvo l t (audio </> f t)
          case z of
            Left e -> do
              trace $ "Something went wrong with forvo, skipping forvo for the remainder of run."
              put @(Maybe ForvoSpec) Nothing
            Right x -> return ()
      return $ genForvos bs is (map f cs)

runPronunciationSpecIO :: Members '[ FBFileSystem
                                   , Trace
                                   , Input ResourceDirs
                                   , FSPKVStore
                                   , Error JSONException
                                   , Error SomeException
                                   , RemoteHttpRequest] m
                        => PronunciationSpec
                        -> Sem m [RForvoNote]
runPronunciationSpecIO (PronunciationSpec f ms a) = evalState @(Maybe ForvoSpec) a $ do
                                                           zs <- forM ms $ runMultiClozeSpecIO f
                                                           return $ join zs

runMinimalReversed :: MinimalReversedSpec -> Sem m RMinimalNoteVF
runMinimalReversed MinimalReversedSpec{..} = return $ val @"from" from :& val @"to" to :& RNil

runBasicReversed :: BasicReversedSpec -> Sem m RBasicReversedNoteVF
runBasicReversed BasicReversedSpec{..} = return $ val @"from" from :& val @"from-extra" from_extra :& val @"to" to :& val @"to-extra" to_extra :& RNil

runSomeSpec :: Members [ RemoteHttpRequest
                       , Trace
                       , Error JSONException
                       , FBFileSystem
                       , ClipProcess
                       , Input ResourceDirs
                       , Error SubtitleParseException
                       , Error SomeException
                       , Input ExportDirs
                       , YouTubeDL
                       , FSPKVStore] m => Spec -> Sem m [SomeNote]
runSomeSpec p = case p of
      Excerpt xs         -> fmap SomeNote <$> (join <$> mapM runExcerptSpecIO xs)
      Pronunciation xs   -> fmap (fmap SomeNote) . runPronunciationSpecIO $ xs
      MinimalReversed xs -> mapM (fmap SomeNote . runMinimalReversed) xs
      BasicReversed xs   -> mapM (fmap SomeNote . runBasicReversed) xs

runMakeDeck :: Deck -> IO ()
runMakeDeck Deck{..} = do
  let ResourceDirs{..} = resourceDirs
  let ExportDirs{..} = exportDirs
  forM_ parts \(Part out p) -> do
    z <- runM
       . traceToIO
       . runError @SomeException
       . mapError @JSONException SomeException
       . mapError @JSONParseException SomeException
       . mapError @SubtitleParseException SomeException
       . runInputConst @ResourceDirs resourceDirs
       . runInputConst @ExportDirs exportDirs
       . interpretFBFileSystem
       . interpretRemoteHttpRequest
       . interpretYouTubeDL
       . runInputConst (JSONFileStore $(mkRelFile ".forvocache"))
       . runKVStoreAsJSONFileStore
       . interpretFFMpegCli
       $ runSomeSpec p
    case z of
      Left x -> throwIO x
      Right x -> do
        S.mktree . S.decodeString . toFilePath $ notes
        writeFileUtf8 (toFilePath (notes </> out)) $ T.intercalate "\n" $ renderNote <$> x

main :: IO ()
main = do
  x <- D.input D.auto "./index.dhall"
  mapM_ runMakeDeck $ fmap snd . Map.toList . decks $ x
