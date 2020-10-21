{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Colog.Polysemy
import           Colog.Polysemy.Formatting
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

import           FlashBlast.ClozeParse
import           FlashBlast.Config
import           FlashBlast.FBFileSystem
import           FlashBlast.Conventions
import           FlashBlast.ForvoClient hiding (id)
import           FlashBlast.JSONFileStore
import           FlashBlast.YouTubeDL
import           Polysemy.State
import           RIO                      hiding (Reader, ask, asks, log, many,
                                           runReader, trace, logInfo)
import           RIO.List
import qualified RIO.Map                  as Map
import qualified RIO.Text                 as T
import Formatting
import qualified Text.Subtitles.SRT       as SR


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

downloadMP3For :: Members [FSPKVStore, ForvoClient, Log (Msg Severity)] r => Locale -> Text -> Sem r (Maybe ByteString)
downloadMP3For l@(Locale l') t = do
  a <- lookupKV @(Locale, Text) @ForvoStandardPronunciationResponseBody (l, t)
  case a of
    Just x -> do
      logInfo ("Response for " % accessed fst stext <> ", " % accessed snd stext % " found in cache.") (l', t)
      p x
    Nothing -> do
      x <- standardPronunciation l t
      updateKV @(Locale, Text) @ForvoStandardPronunciationResponseBody (l,t) $ Just x
      p x
  where
    p :: Members '[ForvoClient] r => ForvoStandardPronunciationResponseBody -> Sem r (Maybe ByteString)
    p ForvoStandardPronunciationResponseBody{..} = case items of
      []      -> return Nothing
      (x':_) -> Just <$> mP3For x'

getForvo :: Members '[Log (Msg Severity), FBFileSystem, FSPKVStore, ForvoClient] r => Locale -> Text -> Path Rel File -> Sem r ()
getForvo l t f = do
  z <- doesFileExist f
  case z of
    True  -> logInfo (accessed id stext % " found in filesystem.") (toFilePathText f)
    False -> do
      logInfo (accessed id stext % " not found in filesystem.") (toFilePathText f)
      x <- downloadMP3For l t
      case x of
        Just x' -> do
          createDirectory (parent f)
          writeFileBS f x'
        Nothing -> return ()

data Toggle a = Toggle Bool
  deriving (Eq, Show, Generic)

data ToggleKilled a = ToggleKilled
  deriving (Eq, Show, Generic)

data ForvoEnabled

errorKillsForvoToggle :: forall a e r b. Members '[State (Toggle a), Log (Msg Severity)] r => Sem (Error e ': r) b -> Sem r ()
errorKillsForvoToggle = runError >=> \case
    Left _ -> do
      logInfo $ "Something went wrong with forvo. Turning forvo off for remainer of run."
      put @(Toggle a) $ Toggle False
    Right _ -> return ()


runMultiClozeSpecIO :: Members '[ Log (Msg Severity)
                                , Input ResourceDirs
                                , FBFileSystem
                                , FSPKVStore
                                , ForvoClient
                                , State (Toggle ForvoEnabled)] m
                    => (Text -> Path Rel File)
                    -> Maybe ForvoSpec
                    -> MultiClozeSpec
                    -> Sem m [RForvoNote]
runMultiClozeSpecIO f s (MultiClozeSpec p is) = do
    ResourceDirs{..} <- input @ResourceDirs
    forM p \a -> do
      let (bs, cs) = genClozePhrase a
      Toggle k <- get @(Toggle ForvoEnabled)
      when k $ do
        forM_ s $ \(ForvoSpec l) ->
          forM cs $ \t -> getForvo l t (audio </> f t)
      return $ genForvos bs is (map f cs)

runPronunciationSpecIO :: Members '[ FBFileSystem
                                   , Input ResourceDirs
                                   , Log (Msg Severity)
                                   , FSPKVStore
                                   , ForvoClient
                                   , State (Toggle ForvoEnabled)
                                   ] m
                        => PronunciationSpec
                        -> Sem m [RForvoNote]
runPronunciationSpecIO (PronunciationSpec f ms a) = do
                                                     zs <- forM ms $ runMultiClozeSpecIO f a
                                                     return $ join zs

runMinimalReversed :: MinimalReversedSpec -> Sem m RMinimalNoteVF
runMinimalReversed MinimalReversedSpec{..} = return $ val @"from" from :& val @"to" to :& RNil

runBasicReversed :: BasicReversedSpec -> Sem m RBasicReversedNoteVF
runBasicReversed BasicReversedSpec{..} = return $ val @"from" from :& val @"from-extra" from_extra :& val @"to" to :& val @"to-extra" to_extra :& RNil

runSomeSpec :: Members [ Log (Msg Severity)
                       , FBFileSystem
                       , ClipProcess
                       , ForvoClient
                       , State (Toggle ForvoEnabled)
                       , Input ResourceDirs
                       , Error SubtitleParseException
                       , Input ExportDirs
                       , YouTubeDL
                       , FSPKVStore] m => Spec -> Sem m [SomeNote]
runSomeSpec p = case p of
      Excerpt xs         -> fmap SomeNote <$> (join <$> mapM runExcerptSpecIO xs)
      Pronunciation xs   -> fmap (fmap SomeNote) . runPronunciationSpecIO $ xs
      MinimalReversed xs -> mapM (fmap SomeNote . runMinimalReversed) xs
      BasicReversed xs   -> mapM (fmap SomeNote . runBasicReversed) xs

runMakeDeck :: Members [ Log (Msg Severity)
                       , Error JSONException
                       , FBFileSystem
                       , ClipProcess
                       , ForvoClient
                       , State (Toggle ForvoEnabled)
                       , Error SubtitleParseException
                       , Error SomeException
                       , YouTubeDL
                       , FSPKVStore] m => Deck -> Sem m ()
runMakeDeck Deck{..} = do
  let ExportDirs{..} = exportDirs
  runInputConst @ResourceDirs resourceDirs $
    runInputConst @ExportDirs exportDirs $
      forM_ parts \(Part out p) -> do
        x <- runSomeSpec p
        writeFileUTF8 (notes </> out) $ T.intercalate "\n" $ renderNote <$> x

main :: IO ()
main = do
   FlashBlastConfig{..} <- D.input D.auto "./index.dhall"
   logEnvStderr <- newLogEnv stderr
   x <- runM
      . traceToIO
      . runLogAction (logTextStderr & cmap (renderThreadTimeMessage logEnvStderr))
      . addThreadAndTimeToLog
      . evalState @(Toggle ForvoEnabled) (Toggle True)
      . runError @SomeException
      . mapError @JSONParseException SomeException
      . mapError @SubtitleParseException SomeException
      . interpretFBFileSystem
      . errorKillsForvoToggle @ForvoEnabled @JSONException
      . interpretFFMpegCli
      . interpretYouTubeDL
      . runInputConst (JSONFileStore $(mkRelFile ".forvocache"))
      . runKVStoreAsJSONFileStore
      . mapError @JSONException SomeException
      . mapError @BadRequestException SomeException
      . interpretRemoteHttpRequest
      . mapError @ForvoResponseNotUnderstood SomeException
      . errorKillsForvoToggle @ForvoEnabled @ForvoLimitReachedException
      . runInputConst @ForvoAPIKey (maybe (ForvoAPIKey "") RIO.id forvoApiKey)
      . interpretForvoClient
      $ mapM_ runMakeDeck $ fmap snd . Map.toList $ decks
   case x of
     Left e -> throwIO e
     Right x' -> return x'
