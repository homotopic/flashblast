{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

import           Composite.Record
import qualified Data.Attoparsec.Text as A
import Data.Aeson
import qualified Dhall                as D
import           Path
import           Path.Dhall           ()
import Network.HTTP.Simple
import           Path.Utils
import           Polysemy
import           Polysemy.Error       as P
import Polysemy.Input
import           Polysemy.Video
import Polysemy.KVStore

import           RIO                  hiding (Reader, ask, asks, many,
                                       runReader)
import           RIO.List
import           RIO.List.Partial
import qualified RIO.Map              as Map
import qualified RIO.Text             as T
import qualified Text.Subtitles.SRT   as SR
import qualified Turtle               as S
import           FlashBlast.AnkiDB
import           FlashBlast.ClozeParse
import           FlashBlast.Conventions
import FlashBlast.ForvoClient
import FlashBlast.YouTubeDL
import qualified UnliftIO.Path.Directory as U
import qualified System.IO.Temp as U
import FlashBlast.Config
import qualified RIO.ByteString as BS
import FlashBlast.JSONFileStore


fromTime :: SR.Time -> Time
fromTime (SR.Time h m s f) = Time h m s f

fromRange :: SR.Range -> Range
fromRange (SR.Range f t) = Range (fromTime f) (fromTime t)

data FBFileSystem m a where
  CreateTempDirectory :: FBFileSystem m (Path Abs Dir)
  CreateDirectory :: Path b Dir -> FBFileSystem m ()
  RemoveDirectory :: Path b Dir -> FBFileSystem m ()
  DoesFileExist :: Path b File -> FBFileSystem m Bool
  CopyFile :: Path b File -> Path b' File -> FBFileSystem m ()
  WriteFileBS :: Path b File -> BS.ByteString -> FBFileSystem m ()

makeSem ''FBFileSystem

interpretFBFileSystem :: Member (Embed IO) effs => Sem (FBFileSystem ': effs) a -> Sem effs a
interpretFBFileSystem = interpret \case
  CreateTempDirectory -> do
    x <- embed $ U.getCanonicalTemporaryDirectory
    embed $ U.createTempDirectory x "" >>= parseAbsDir
  CreateDirectory x -> U.createDirectoryIfMissing True x
  RemoveDirectory x   -> U.removeDirectoryRecursive x
  DoesFileExist x     -> U.doesFileExist x
  CopyFile x y -> U.copyFile x y
  WriteFileBS x y -> BS.writeFile (toFilePath x) y

runExcerptSpecIO :: Members '[Error SubtitleParseException
                       , FBFileSystem
                       , Input ExportDirs
                       , Input ResourceDirs
                       , YouTubeDL
                       , ClipProcess] m
                       => ExcerptSpec -> Sem m [RExcerptNote]
runExcerptSpecIO (ExcerptSpec {..}) = do
  ResourceDirs{..} <- input @ResourceDirs
  ExportDirs{..} <- input @ExportDirs
  t <- case source of
    YouTubeDL (YDLInfo x y f) -> do
      youTubeDL' x (video </> y) f
      return (video </> y)
    LocalVideo x -> return (video </> x)
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
  when (not . null $ cs') $ do
    extractClips t $ zip (fromRange . SR.range <$> s') (h </$> cs')
    forM_ cs' $ \x -> copyFile (h </> x) (clips </> x)
  when (not . null $ es') $ do
    extractAudio t $ zip (fromRange . SR.range <$> s') (h </$> es)
    forM_ es' $ \x -> copyFile (h </> x) (audio </> x)
  removeDirectory h
  forM (zip4 s' cs es fs) $ \(l, c, e, f) -> do
    whenM (fmap not . doesFileExist $ images </> f) $
      extractFrames (clips </> c) $ [(Time 0 0 0 0, images </> f)]
    return $ val @"front" (fst . genClozePhrase . SR.dialog $ l)
          :& val @"extra" f
          :& val @"back"  e
          :& RNil

newtype SubtitleParseException = SubtitleParseException String
  deriving (Eq, Show, Generic)

instance Exception SubtitleParseException

type FSPKVStore = KVStore (Locale, Text) ForvoStandardPronunciationResponseBody

downloadMP3For :: Members [FSPKVStore, ForvoClient] r => Locale -> Text -> Sem r (Maybe ByteString)
downloadMP3For l t = do
  a <- lookupKV @(Locale, Text) @ForvoStandardPronunciationResponseBody (l, t)
  case a of
    Just x -> p x
    Nothing -> do
      x <- standardPronunciation l t
      updateKV @(Locale, Text) @ForvoStandardPronunciationResponseBody (l,t) $ Just x
      p x
  where
    p :: Members '[ForvoClient] r => ForvoStandardPronunciationResponseBody -> Sem r (Maybe ByteString)
    p x = case items x of
      [] -> return Nothing
      (x':xs) -> Just <$> mP3For x'

getForvo :: Members '[FBFileSystem, FSPKVStore, ForvoClient] r => Locale -> Text -> Path Rel File -> Sem r ()
getForvo l t f = do
  unlessM (doesFileExist f) $ do
    x <- downloadMP3For l t
    case x of
      Just x' -> writeFileBS f x'
      Nothing -> return ()

runMultiClozeSpecIO :: Members '[Embed IO, Error SomeException, FBFileSystem, Error JSONException, FSPKVStore, Input (Maybe ForvoSpec)] m => (Text -> Path Rel File) -> MultiClozeSpec -> Sem m [RForvoNote]
runMultiClozeSpecIO f (MultiClozeSpec p is) =
    forM p \a -> do
      let (bs, cs) = genClozePhrase a
      s <- input @(Maybe ForvoSpec)
      forM s $ \(ForvoSpec l api) ->
        forM cs $ \t -> runInputConst @ForvoAPIKey api . interpretForvoClient $ getForvo l t (f t)
      return $ genForvos bs is (map f cs)

runPronunciationSpecIO :: Members '[FBFileSystem, FSPKVStore, Error JSONException, Error SomeException, Embed IO] m => PronunciationSpec -> Sem m [RForvoNote]
runPronunciationSpecIO (PronunciationSpec f ms a) = runInputConst @(Maybe ForvoSpec) a $ do
                                                           zs <- forM ms $ runMultiClozeSpecIO f
                                                           return $ join zs

runMinimalReversed :: MinimalReversedSpec -> Sem m RMinimalNoteVF
runMinimalReversed MinimalReversedSpec{..} = return $ val @"from" from :& val @"to" to :& RNil

runBasicReversed :: BasicReversedSpec -> Sem m RBasicReversedNoteVF
runBasicReversed BasicReversedSpec{..} = return $ val @"from" from :& val @"from-extra" from_extra :& val @"to" to :& val @"to-extra" to_extra :& RNil

runSomeSpec :: Members [Embed IO, Error JSONException, FBFileSystem, ClipProcess, Input ResourceDirs, Error SubtitleParseException, Error SomeException, Input ExportDirs, YouTubeDL, FSPKVStore] m => Spec -> Sem m [SomeNote]
runSomeSpec p = case p of
      Excerpt xs         -> fmap (fmap SomeNote) $ (join <$> mapM runExcerptSpecIO xs)
      Pronunciation xs   -> fmap (fmap SomeNote) . runPronunciationSpecIO $ xs
      MinimalReversed xs -> mapM (fmap SomeNote . runMinimalReversed) xs
      BasicReversed xs   -> mapM (fmap SomeNote . runBasicReversed) xs

runMakeDeck :: Deck -> IO ()
runMakeDeck Deck{..} = do
  let ResourceDirs{..} = resourceDirs
  let ExportDirs{..} = exportDirs
  forM_ parts \(Part out p) -> do
    z <- runM
       . runError @SomeException
       . mapError @JSONParseException SomeException
       . mapError @JSONException SomeException
       . mapError @SubtitleParseException SomeException
       . runInputConst @ResourceDirs resourceDirs
       . runInputConst @ExportDirs exportDirs
       . interpretFBFileSystem
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
