{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

import           Composite.Record
import qualified Data.Attoparsec.Text as A
import           Dhall                hiding (embed, string)
import           Path
import           Path.Dhall           ()
import           Path.Utils
import           Polysemy
import           Polysemy.Error       as P
import           Polysemy.Reader
import           Polysemy.Video

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
import qualified UnliftIO.Path.Directory as U
import qualified System.IO.Temp as U


data MultiClozeSpec = MultiClozeSpec {
  phrases :: [Text]
, images  :: [Path Rel File]
} deriving (Eq, Generic, Show)

instance FromDhall MultiClozeSpec

data YDLInfo = YDLInfo {
  url :: Text
, out :: Path Rel File
, format :: Text
} deriving (Eq, Generic, Show)

instance FromDhall YDLInfo
data VideoSource = LocalVideo (Path Rel File) | YouTubeDL YDLInfo
  deriving (Eq, Generic, Show)

instance FromDhall VideoSource

data ExcerptSpec = ExcerptSpec {
  source :: VideoSource
, subs  :: Text
, clipf :: Text -> Text
, audiof :: Text -> Text
, framef :: Text -> Text
} deriving Generic

instance FromDhall ExcerptSpec

data Locale = Locale Text
  deriving (Eq, Show, Generic)

data ExportDirs = ExportDirs {
  audio  :: Path Rel Dir
, clips  :: Path Rel Dir
, images :: Path Rel Dir
, notes  :: Path Rel Dir
} deriving (Eq, Show, Generic)

instance FromDhall ExportDirs

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

data YouTubeDL m a where
  YouTubeDL' :: Text -> Path Rel File -> Text -> YouTubeDL m ()

makeSem ''YouTubeDL

interpretYouTubeDL :: Member (Embed IO) effs => Sem (YouTubeDL ': effs) a -> Sem effs a
interpretYouTubeDL = interpret \case
  YouTubeDL' x k f -> S.sh $ S.inproc "youtube-dl" [x, "-o", toFilePathText k, "-f", f] mempty

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

genForvos :: MonadThrow m => Locale -> Text -> [Path Rel File] -> [Text] -> m RForvoNote
genForvos (Locale l) x zs as = do
  ys' <- mapM (forvoConvention l) as
  let ys = lpadZipWith (\a _ -> if isJust a then a else Nothing) ys' (replicate 16 ())
  let k = ys !! 0 :*: ys !! 1 :*: ys !! 2 :*: ys !! 3 :*: ys !! 4 :*: ys !! 5 :*: ys !! 6 :*: ys !! 7 :*: ys !! 8 :*: ys !! 9 :*: ys !! 10 :*: ys !! 11 :*: ys !! 12 :*: ys !! 13 :*: ys !! 14 :*: ys !! 15 :*: RNil
  return $ x :*: zs :*: k

runMultiClozeSpecIO :: Locale -> ResourceDirs -> ExportDirs -> [MultiClozeSpec] -> Path Rel File -> IO ()
runMultiClozeSpecIO l _ x xs out = do
  zs <- forM xs \(MultiClozeSpec p f) -> do
    forM p \a -> let (b, c) = genClozePhrase a
                 in  genForvos l b f c
  S.mktree . S.decodeString . toFilePath $ (notes x)
  writeFileUtf8 (toFilePath (notes x </> out)) $ (T.intercalate "\n" $ renderForvoNote <$> join zs)

data ResourceDirs = ResourceDirs {
  audio  :: Path Rel Dir
, video  :: Path Rel Dir
, images :: Path Rel Dir
} deriving (Eq, Show, Generic)

instance FromDhall ResourceDirs

data Deck = Deck {
  resourceDirs :: ResourceDirs
, exportDirs   :: ExportDirs
, parts        :: [Part]
} deriving Generic

data Part = Part {
  outfile :: Path Rel File
, spec   :: Spec
} deriving Generic

instance FromDhall Deck

instance FromDhall Part

data BasicReversedSpec = BasicReversedSpec {
  from       :: VF
, from_extra :: VF
, to         :: VF
, to_extra   :: VF
} deriving (Eq, Show, Generic)

instance FromDhall BasicReversedSpec

instance FromDhall Locale

data ForvoSpec = ForvoSpec {
  locale :: Locale
, spec :: [MultiClozeSpec]
} deriving (Eq, Show, Generic)

instance FromDhall ForvoSpec
data Spec =
    Forvo ForvoSpec
  | Excerpt [ExcerptSpec]
  | BasicReversed [BasicReversedSpec]
  | MinimalReversed [MinimalReversedSpec]
    deriving Generic

instance FromDhall Spec

data WonkyConfig = WonkyConfig {
  decks :: Map Text Deck
} deriving Generic

instance FromDhall WonkyConfig

data MinimalReversedSpec = MinimalReversedSpec {
  from :: VF
, to   :: VF
} deriving (Eq, Show, Generic)

instance FromDhall MinimalReversedSpec

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
    Forvo (ForvoSpec l x)   -> runMultiClozeSpecIO l resourceDirs exportDirs x a
    MinimalReversed x -> runMinimalReversedIO resourceDirs exportDirs x a
    BasicReversed x -> runBasicReversedIO resourceDirs exportDirs x a

main :: IO ()
main = do
  x <- input auto "./index.dhall"
  mapM_ runMakeDeck $ fmap snd . Map.toList . decks $ x
