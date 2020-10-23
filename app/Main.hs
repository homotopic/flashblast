{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Colog.Polysemy
import FlashBlast.Domain
import RIO.Time
import Colog.Polysemy.Formatting.LogEnv
import Colog.Polysemy.Formatting.ThreadTimeMessage
import Colog.Polysemy.Formatting.Color
import Colog.Polysemy.Formatting.Render
import Data.Text.Lazy.Builder (Builder)
import           Colog.Polysemy.Formatting
import           Composite.Record
import qualified Data.Attoparsec.Text     as A
import qualified Dhall                    as D
import           Network.HTTP.Simple
import Polysemy.Output
import           Path
import           Path.Dhall               ()
import           Path.Utils
import           Polysemy
import           Polysemy.Error           as P
import           Polysemy.Input
import           Polysemy.KVStore
import           Polysemy.Video hiding (to)

import           FlashBlast.ClozeParse
import qualified FlashBlast.Config as Config
import           FlashBlast.FS
import           FlashBlast.Conventions
import           FlashBlast.ForvoClient hiding (id)
import           FlashBlast.JSONFileStore
import           FlashBlast.YouTubeDL
import           Polysemy.State
import           RIO.List
import RIO hiding(view, to, Builder, logInfo)
import qualified RIO.Map                  as Map
import qualified RIO.Text                 as T
--import Formatting
--import Formatting.Time
import Polysemy.Tagged
import qualified Text.Subtitles.SRT       as SR
import Optics

{--
fromTime :: SR.Time -> Time
fromTime (SR.Time h m s f) = Time h m s f

fromRange :: SR.Range -> Range
fromRange (SR.Range f t) = Range (fromTime f) (fromTime t)

fFieldsGreenBarSep :: UseColor -> Format r ([Builder] -> r)
fFieldsGreenBarSep useColor = later $ \fields ->
  let withFG = getWithFG useColor
      sep = format builder $ withFG Green " | "
  in bformat (intercalated sep builder) fields

interpretVideoSource :: Members '[Input ResourceDirs, YouTubeDL] m => VideoSource -> Sem m (Path Rel File)
interpretVideoSource = \case
  Dhall.YouTubeDL (YDLInfo x y f) -> do
    ResourceDirs{..} <- input @ResourceDirs
    youTubeDL' x (video </> y) f
    return (video </> y)
  LocalVideo x -> do
    ResourceDirs{..} <- input @ResourceDirs
    return (video </> x)

runExcerptSpecIO :: Members '[Error SubtitleParseException
                       , FSExist
                       , FSTemp
                       , FSCopy
                       , FSDir
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

getForvo :: Members '[ Log (Msg Severity)
                     , FSKVStore Rel
                     , FSPKVStore
                     , ForvoClient] r
         => Locale -> Text -> Path Rel File -> Sem r ()
getForvo l t f = do
  z <- lookupKV f
  case z of
    Just _ -> return ()
    Nothing -> do
      x' <- downloadMP3For l t
      updateKV f x'

data Toggle a = Toggle Bool
  deriving (Eq, Show, Generic)

data ToggleKilled a = ToggleKilled
  deriving (Eq, Show, Generic)

data ForvoEnabled

fIso8601 :: FormatTime a => (Color -> Builder -> Builder) -> Format r (a -> r)
fIso8601 withFG = later $ \time -> mconcat
  [ bformat dateDash time
  , withFG Green "T"
  , withFG Yellow $ bformat hms time
  ]

renderThreadTimeMessage' :: LogEnv -> ThreadTimeMessage -> T.Text
renderThreadTimeMessage' (LogEnv useColor zone) (ThreadTimeMessage threadId time (Msg severity stack message)) =
  let withFG = getWithFG useColor
  in sformat (fFieldsGreenBarSep useColor)
    [ bformat (fSeverity withFG) severity
    , bformat (fIso8601 withFG) (utcToZonedTime zone time)
    , bformat stext message
    ]
--}
{--
runMultiClozeSpecIO :: Members '[ Log (Msg Severity)
                                , Input ResourceDirs
                                , FSPKVStore
                                , FSWrite
                                , FSRead
                                , FSExist
                                , FSDir
                                , ForvoClient] m
                    => (Text -> Path Rel File)
                    -> Maybe ForvoSpec
                    -> MultiClozeSpec
                    -> Sem m [RForvoNote]
runMultiClozeSpecIO f s (MultiClozeSpec p is) = do
    ResourceDirs{..} <- input @ResourceDirs
    forM p \a -> let (bs, cs) = genClozePhrase a
                 in genForvos bs is (map f cs)

runPronunciationSpecIO :: Members '[Input ResourceDirs
                                   , Log (Msg Severity)
                                   , FSPKVStore
                                   , ForvoClient
                                   , FSWrite
                                   , FSExist
                                   , FSRead
                                   , FSDir
                                   , State (Toggle ForvoEnabled)
                                   ] m
                        => PronunciationSpec
                        -> Sem m [RForvoNote]
runPronunciationSpecIO (PronunciationSpec f ms a) = do
                                                     zs <- forM ms $ runMultiClozeSpecIO f a
                                                     return $ join zs



runSomeSpec :: Members [ Log (Msg Severity)
                       , ClipProcess
                       , ForvoClient
                       , State (Toggle ForvoEnabled)
                       , Input ResourceDirs
                       , Error SubtitleParseException
                       , Input ExportDirs
                       , YouTubeDL
                       , FSTemp
                       , FSExist
                       , FSWrite
                       , FSDir
                       , FSRead
                       , FSCopy
                       , FSPKVStore] m => Spec -> Sem m [SomeNote]
runSomeSpec p = case p of
      Excerpt xs         -> fmap SomeNote <$> (join <$> mapM runExcerptSpecIO xs)
      Pronunciation xs   -> fmap (fmap SomeNote) . runPronunciationSpecIO $ xs
      MinimalReversed xs -> mapM (fmap SomeNote . runMinimalReversed) xs
      BasicReversed xs   -> mapM (fmap SomeNote . runBasicReversed) xs

runMakeDeck :: Members [ Log (Msg Severity)
                       , ClipProcess
                       , ForvoClient
                       , State (Toggle ForvoEnabled)
                       , Error SubtitleParseException
                       , YouTubeDL
                       , FSWrite
                       , FSExist
                       , FSTemp
                       , FSDir
                       , FSCopy
                       , FSRead
                       , FSPKVStore] m => Deck -> Sem m ()
runMakeDeck Deck{..} = do
  let ExportDirs{..} = exportDirs
  runInputConst @ResourceDirs resourceDirs $
    runInputConst @ExportDirs exportDirs $
      forM_ parts \(Part out p) -> do
        x <- runSomeSpec p
        writeFileUtf8 (notes </> out) $ T.intercalate "\n" $ renderNote <$> x
--}
type FSKVStore b = KVStore (Path b File) ByteString

runFSKVStoreRelIn :: Members '[FSExist, FSRead, FSWrite, FSDir] r => Path b Dir -> Sem (FSKVStore Rel ': r) a -> Sem r a
runFSKVStoreRelIn d = interpret \case
  LookupKV k   -> do
    createDirectory d
    z <- doesFileExist (d </> k)
    case z of
      True  -> fmap Just . readFileBS $ d </> k
      False -> return Nothing
  UpdateKV k v -> do
    createDirectory d
    case v of
      Nothing -> pure ()
      Just x  -> writeFileBS (d </> k) x

{--
data PronunciationDictionary m a where
  DiscoverClozePhrases       :: MultiClozeSpec -> PronunciationDictionary m [(Locale, Text)]
  DoesPronunciationFileExist :: (Locale, Text) -> PronunciationDictionary m Bool
  FetchPronunciationData     :: (Locale, Text) -> PronunciationDictionary m ByteString
  WritePronunciationData     :: (Locale, Text) -> ByteString -> PronunciationDictionary m ()

makeSem ''PronunciationDictionary

fillPronunciationDictionary :: Members '[PronunciationDictionary] r => PronunciationSpec -> Sem r ()
filPronunciationDictionary  = do
  zs <- discoverClozePhrases x
  forM zs $ \z -> do
    x <- doesPronunciationFileExist z
    case x of
      True  -> return ()
      False -> do
        y <- fetchPronunciationData z
        writePronunciationData z
--}

{-
extractClozes :: Text -> [Text]
extractClozes = snd . genClozePhrase

getForvoPrep :: Deck -> [(ForvoSpec, (Text -> Path Rel File), [Text])]
getForvoPrep (Deck{..}) = join $ map (\x ->
  case x of
    Pronunciation (PronunciationSpec f ms (Just z)) -> [(z, f, join $ map extractClozes (join $ map phrases ms))]
    _ -> []
  ) (map spec parts)

hotKVStore :: forall t t' k v r. Members '[Input k, Tagged t (KVStore k v), Tagged t' (KVStore k v)] r => Sem r ()
hotKVStore = do
  a <- input @k
  z <- tag @t @(KVStore k v) $ existsKV @k @v a
  if z
    then return ()
  else
    do
      x <- tag @t' @(KVStore k v) $ lookupKV @k @v a
      case x of
        Nothing -> return ()
        Just x' -> tag @t @(KVStore k v) $ writeKV @k @v a x'
--}
runKVStoreAsKVStore :: forall k v k' v' r a. Getter k k' -> Iso' v v' -> Sem (KVStore k v ': r) a -> Sem (KVStore k' v' ': r) a
runKVStoreAsKVStore f g = reinterpret \case
  LookupKV k   -> fmap (review g) <$> lookupKV @k' @v' (view f k)
  UpdateKV k x -> updateKV @k' @v' (view f k) (fmap (view g) x)

data Deck = Deck {
  notes :: [Path Rel File]
, media :: [Path Rel File]
} deriving (Eq, Show, Generic)

generateMinimalReversedNoteVF :: Config.MinimalReversedCard -> RMinimalNoteVF
generateMinimalReversedNoteVF Config.MinimalReversedCard{..} = val @"front" _front
                                                            :& val @"back"  _back
                                                            :& RNil

generateBasicReversedNoteVF :: Config.BasicReversedCard -> RBasicReversedNoteVF
generateBasicReversedNoteVF Config.BasicReversedCard{..} = val @"front"       _front
                                                        :& val @"front-extra" _front_extra
                                                        :& val @"back"        _back
                                                        :& val @"back-extra"  _back_extra
                                                        :& RNil

makeDeck :: Members '[] r
         => Config.Deck
         -> Sem r Deck
makeDeck x = do
  let f = itoListOf
            ( Config.parts
            % itraversed
            % Config.spec
            % Config._Pronunciation
            % Config.multis
            % traversed
            % Config.phrases
            ) x
  let a = itoListOf
            ( Config.parts
            % itraversed
            % Config.spec
            % Config._BasicReversed
            ) x
  return $ Deck [] []

--}
{--
live :: (Text -> Path Rel File) -> IO ()
live f = hotKVStore @"local" @"remote" @Text @ByteString
          & untag @"local" @(KVStore Text ByteString)
          & runKVStoreAsKVStore @Text @ByteString @(Path Rel File) @ByteString (to f) (iso id id)
          & runFSKVStoreRelIn $(mkRelDir "foo")
          & untag @"remote" @(KVStore Text ByteString)
          & runForvoClient
          & runError @ForvoAPIKeyIncorrectException
          & runError @ForvoLimitReachedException
          & runError @ForvoResponseNotUnderstood
          & runRemoteHttpRequest
          & runError @JSONException
          & runError @BadRequestException
          & runInputConst
          & runFSReadIO
          & runFSWriteIO
          & runFSExistIO
          & runFSDirIO
          & runFSTempIO
          & runM

test :: IO ()
test = hotKVStore @"foo" @"bar" @Char @Int
        & runInputConst @Char 'a'
        & untag @"bar" @(KVStore Char Int)
        & runKVStoreAsState
        & evalState (Map.fromList [('a', 1)])
        & untag @"foo" @(KVStore Char Int)
        & runKVStoreAsState
        & evalState (Map.fromList [])
        & runM
--}
--
main :: IO ()
main = flashblast @Config.Deck @Deck makeDeck
        & untag @DeckConfiguration
        & runInputConst (Config.Deck { })
--        & runKVStoreAsState @(Locale, Text) @(Path Rel File)
  --      & evalState (Map.fromList [])
        & untag @CollectionsPackage
        & runOutputSem (embed . traceShowM)
        & runM
{--
main :: IO ()
main = do
   FlashBlastConfig{..} <- D.input D.auto "./index.dhall"
   logEnvStderr <- newLogEnv stderr
   let logT = logTextStderr & cmap (renderThreadTimeMessage' logEnvStderr)
   runM $ do
     x <- runError @ForvoLimitReachedException
        . runInputConst @ForvoAPIKey (maybe (ForvoAPIKey "") RIO.id forvoApiKey)
        . interpretForvoClient
        $ forM (Map.toList $ decks) $ \(n, x@Deck{..}) -> do
            let x' = getForvoPrep x
            forM x' $ \(ForvoSpec{..}, f, zs) -> forM zs $ \t -> getForvo locale t (f t)
{--   x <- runM
      . evalState @(Toggle ForvoEnabled) (Toggle True)
      . runFSExistIO
      . runFSReadIO
      . runFSWriteIO
      . runFSDirIO
      . runFSTempIO
      . runFSCopyIO
      . runLogAction  logT
      . addThreadAndTimeToLog
      . mapLog (logInfo' . fileExistsLogText)
      . logFileExists
      . runError @SomeException
      . mapError @JSONParseException SomeException
      . mapError @SubtitleParseException SomeException
      . interpretFFMpegCli
      . interpretYouTubeDL
      . runInputConst (JSONFileStore $(mkRelFile ".forvocache"))
      . runKVStoreAsJSONFileStore
      . mapError @JSONException SomeException
      . mapError @BadRequestException SomeException
      . interpretRemoteHttpRequest
      . mapError @ForvoResponseNotUnderstood SomeException
      . mapError @ForvoAPIKeyIncorrectException SomeException
      . mapError @ForvoLimitReachedException SomeException
      $ mapM_ runMakeDeck $ fmap snd . Map.toList $ decks--}
     case x of
       Left e -> throwIO e
       Right x' -> return x'
       --}
