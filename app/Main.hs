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
import           Polysemy.Reader
import           Polysemy.Video
import Polysemy.KVStore

import           Data.Align
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
import RIO.Time
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

genExcerpts :: Members '[Error SomeException, FBFileSystem, YouTubeDL, ClipProcess, Reader ExportDirs] m => Path Rel Dir -> ExcerptSpec -> Sem m [RExcerptNote]
genExcerpts dir (ExcerptSpec {..}) = do
  t <- case source of
    YouTubeDL (YDLInfo x y f) -> do
      youTubeDL' x (dir </> y) f
      return (dir </> y)
    LocalVideo x -> return (dir </> x)
  ExportDirs{..} <- ask @ExportDirs
  s' <- either (throwM . SubtitleParseException) return $ A.parseOnly SR.parseSRT subs
  cs <- mapM (parseRelFile . T.unpack . clipf . T.pack . show . SR.index) s'
  es <- mapM (parseRelFile . T.unpack . audiof . T.pack . show . SR.index) s'
  fs <- mapM (parseRelFile . T.unpack . framef . T.pack . show . SR.index) s'
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

instance Member (Error SomeException) r => MonadThrow (Sem r) where
  throwM e = P.throw (toException e)

runExcerptSpecIO :: ResourceDirs -> ExportDirs -> [ExcerptSpec] -> Path Rel File -> IO ()
runExcerptSpecIO (ResourceDirs{..}) x xs out = do
  zs <- sequenceA <$> forM xs \k -> do
    runM . runError . runReader x . interpretYouTubeDL . interpretFFMpegCli . interpretFBFileSystem $ genExcerpts video k
  case zs of
    Right a -> do
      S.mktree . S.decodeString . toFilePath $ (notes x)
      writeFileUtf8 (toFilePath (notes x </> out)) $ T.intercalate "\n" $ renderExcerptNote <$> join a
    Left (SomeException p) -> throwIO p

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

genForvos :: (MonadThrow m, MonadIO m) => Text -> [Path Rel File] -> [Path Rel File] -> m RForvoNote
genForvos x zs ys' = do
  let ys = lpadZipWith (\a _ -> if isJust a then a else Nothing) ys' (replicate 16 ())
  let k = ys !! 0 :*: ys !! 1 :*: ys !! 2 :*: ys !! 3 :*: ys !! 4 :*: ys !! 5 :*: ys !! 6 :*: ys !! 7 :*: ys !! 8 :*: ys !! 9 :*: ys !! 10 :*: ys !! 11 :*: ys !! 12 :*: ys !! 13 :*: ys !! 14 :*: ys !! 15 :*: RNil
  return $ x :*: zs :*: k

getForvo :: Members '[FBFileSystem, Input ResourceDirs, Input (Text -> Path Rel File), FSPKVStore, ForvoClient] r => Locale -> Text -> Sem r ()
getForvo l t = do
  ResourceDirs{..} <- input @ResourceDirs
  f <- input @(Text -> Path Rel File)
  whenM (fmap not . doesFileExist $ (audio </> f t)) $ do
    x <- downloadMP3For l t
    case x of
      Just x' -> do
        f <- input @(Text -> Path Rel File)
        writeFileBS (audio </> f t) x'
      Nothing -> return ()

runMultiClozeSpecIO :: Maybe ForvoSpec -> ResourceDirs -> ExportDirs -> [MultiClozeSpec] -> (Text -> Path Rel File) -> Path Rel File -> IO ()
runMultiClozeSpecIO s r@ResourceDirs{..} x xs f' out = do
  traceShowM $ "flsdsad"
  zs <- forM xs \(MultiClozeSpec p is) -> do
    forM p \a -> do
                  let (bs, cs) = genClozePhrase a
                  forM s $ \(ForvoSpec l api) ->
                    forM cs $ \c' -> do
                      runM . runInputConst @JSONFileStore (JSONFileStore $(mkRelFile ".forvocache")) . runError @JSONParseException . runKVStoreAsJSONFileStore . runError @SomeException . runError @JSONException . runInputConst @ForvoAPIKey api . interpretForvoClient . runInputConst @ResourceDirs r . runInputConst @(Text -> Path Rel File) f' . interpretFBFileSystem $ getForvo l c'
                  genForvos bs is (map f' cs)
  S.mktree . S.decodeString . toFilePath $ (notes x)
  writeFileUtf8 (toFilePath (notes x </> out)) $ (T.intercalate "\n" $ renderForvoNote <$> join zs)

runMinimalReversedIO :: ResourceDirs -> ExportDirs -> [MinimalReversedSpec] -> Path Rel File -> IO ()
runMinimalReversedIO _ x xs out = do
  zs <- forM xs \MinimalReversedSpec{..} -> return $ val @"from" from :& val @"to" to :& RNil
  S.mktree . S.decodeString . toFilePath $ notes x
  writeFileUtf8 (toFilePath (notes x </> out)) $ (T.intercalate "\n" $ renderMinimalNoteVF <$> zs)

runBasicReversedIO :: ResourceDirs -> ExportDirs -> [BasicReversedSpec] -> Path Rel File -> IO ()
runBasicReversedIO ResourceDirs{..} x xs out = do
  zs <- forM xs \BasicReversedSpec{..} -> return $ val @"from" from :& val @"from-extra" from_extra :& val @"to" to :& val @"to-extra" to_extra :& RNil
  S.mktree . S.decodeString . toFilePath $ notes x
  writeFileUtf8 (toFilePath (notes x </> out)) $ T.intercalate "\n" (renderBasicReversedNoteVF <$> zs)

runMakeDeck :: Deck -> IO ()
runMakeDeck Deck{..} = do
  forM_ parts \(Part a p) -> case p of
    Excerpt x -> runExcerptSpecIO resourceDirs exportDirs x a
    Pronunciation (PronunciationSpec f x l) -> runMultiClozeSpecIO l resourceDirs exportDirs x f a
    MinimalReversed x -> runMinimalReversedIO resourceDirs exportDirs x a
    BasicReversed x -> runBasicReversedIO resourceDirs exportDirs x a

main :: IO ()
main = do
  x <- D.input D.auto "./index.dhall"
  mapM_ runMakeDeck $ fmap snd . Map.toList . decks $ x
