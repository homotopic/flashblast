{-# LANGUAGE DuplicateRecordFields #-}

import           Colog.Polysemy
import           Colog.Polysemy.Formatting
import           Colog.Polysemy.Formatting.Color
import           Colog.Polysemy.Formatting.LogEnv
import           Colog.Polysemy.Formatting.Render
import           Colog.Polysemy.Formatting.ThreadTimeMessage
import           Composite.Record
import qualified Data.Attoparsec.Text                        as A
import           Data.Monoid.Generic
import           Data.Text.Lazy.Builder                      (Builder)
import qualified Dhall                                       as D
import           Network.HTTP.Simple
import           Path
import           Path.Dhall                                  ()
import           Path.Utils
import           Polysemy
import           Polysemy.Error                              as P
import           Polysemy.Input
import           Polysemy.KVStore
import           Polysemy.Output
import           Polysemy.Several
import           Polysemy.Video                              hiding (to)
import           RIO.Time
import Polysemy.FSKVStore

import           FlashBlast.ClozeParse
import qualified FlashBlast.Config                           as Config
import           FlashBlast.Conventions
import           FlashBlast.Domain
import           FlashBlast.ForvoClient                      hiding (id)
import Data.Functor.Contravariant
import           FlashBlast.YouTubeDL
import Polysemy.FS
import qualified Formatting                                  as F
import           Formatting.Time
import           Optics
import           Polysemy.Methodology
import           Polysemy.Tagged
import           RIO                                         hiding (Builder, trace, log, Display,
                                                              logInfo, over, to,
                                                              view,
                                                              writeFileUtf8,
                                                              (^.))
import           RIO.List
import qualified RIO.Map                                     as Map
import qualified RIO.Text                                    as T
import qualified Text.Subtitles.SRT                          as SR
import Polysemy.Trace

fromTime :: SR.Time -> Time
fromTime (SR.Time h m s f) = Time h m s f

fromRange :: SR.Range -> Range
fromRange (SR.Range f t) = Range (fromTime f) (fromTime t)

fFieldsGreenBarSep :: UseColor -> F.Format r ([Builder] -> r)
fFieldsGreenBarSep useColor = F.later $ \fields ->
  let withFG = getWithFG useColor
      sep = F.format F.builder $ withFG Green " | "
  in F.bformat (F.intercalated sep F.builder) fields

interpretVideoSource :: Members '[Input Config.ResourceDirs, YouTubeDL] m
                     => Config.VideoSource
                     -> Sem m (Path Rel File)
interpretVideoSource = \case
  Config.YouTubeDL (Config.YDLInfo x y f) -> do
    Config.ResourceDirs{..} <- input @Config.ResourceDirs
    youTubeDL' x (_video </> y) f
    return (_video </> y)
  Config.LocalVideo x -> do
    Config.ResourceDirs{..} <- input @Config.ResourceDirs
    return (_video </> x)

runExcerptSpecIO :: Members '[ Error SubtitleParseException
                             , FSExist
                             , FSTemp
                             , FSCopy
                             , FSDir
                             , Input Config.ExportDirs
                             , Input Config.ResourceDirs
                             , YouTubeDL
                             , ClipProcess] m
                 => Config.ExcerptSpec
                 -> Sem m [RExcerptNote]
runExcerptSpecIO Config.ExcerptSpec {..} = do
  Config.ExportDirs{..} <- input @Config.ExportDirs
  t <- interpretVideoSource _source
  s' <- either (throw . SubtitleParseException) return $ A.parseOnly SR.parseSRT _subs
  let cs = map (_clipf  . T.pack . show . SR.index) s'
  let es = map (_audiof . T.pack . show . SR.index) s'
  let fs = map (_framef . T.pack . show . SR.index) s'
  cs' <- filterM (fmap not . doesFileExist . (_clips </>)) cs
  es' <- filterM (fmap not . doesFileExist . (_audio </>)) es
  h <- createTempDirectory
  createDirectory _clips
  createDirectory _audio
  createDirectory _images
  unless (null cs') $ do
    extractClips t $ zip (fromRange . SR.range <$> s') (h </$> cs')
    forM_ cs' $ \x -> copyFile (h </> x) (_clips </> x)
  unless (null es') $ do
    extractAudio t $ zip (fromRange . SR.range <$> s') (h </$> es)
    forM_ es' $ \x -> copyFile (h </> x) (_audio </> x)
  removeDirectory h
  forM (zip4 s' cs es fs) $ \(l, c, e, f) -> do
    whenM (fmap not . doesFileExist $ _images </> f) $
      extractFrames (_clips </> c) [(Time 0 0 0 0, _images </> f)]
    return $ val @"front" (fst . genClozePhrase . SR.dialog $ l)
          :& val @"extra" f
          :& val @"back"  e
          :& RNil

newtype SubtitleParseException = SubtitleParseException String
  deriving (Eq, Show, Generic)

instance Exception SubtitleParseException

downloadMP3For :: Members '[ForvoClient] r => Locale -> Text -> Sem r (Maybe ByteString)
downloadMP3For l@(Locale l') t = do
  ForvoStandardPronunciationResponseBody {..} <- standardPronunciation l t
  case items of
      []     -> return Nothing
      (x':_) -> Just <$> mP3For x'

getForvo :: Members '[ FSKVStore Rel ByteString
                     , ForvoClient] r
         => Locale -> Text -> Path Rel File -> Sem r ()
getForvo l t f = do
  z <- lookupKV f
  case z of
    Just _ -> return ()
    Nothing -> do
      x' <- downloadMP3For l t
      updateKV f x'

fIso8601 :: FormatTime a => (Color -> Builder -> Builder) -> F.Format r (a -> r)
fIso8601 withFG = F.later $ \time -> mconcat
  [ F.bformat dateDash time
  , withFG Green "T"
  , withFG Yellow $ F.bformat hms time
  ]

renderThreadTimeMessage' :: LogEnv -> ThreadTimeMessage -> T.Text
renderThreadTimeMessage' (LogEnv useColor zone) (ThreadTimeMessage threadId time (Msg severity stack message)) =
  let withFG = getWithFG useColor
  in F.sformat (fFieldsGreenBarSep useColor)
    [ F.bformat (fSeverity withFG) severity
    , F.bformat (fIso8601 withFG) (utcToZonedTime zone time)
    , F.bformat F.stext message
    ]


runMultiClozeSpecIO :: Members '[ FSWrite
                                , FSRead
                                , FSExist
                                , FSDir] m
                    => (Text -> Path Rel File)
                    -> Maybe Config.ForvoSpec
                    -> Config.MultiClozeSpec
                    -> Sem m [RForvoNote]
runMultiClozeSpecIO f s (Config.MultiClozeSpec p is) = do
    forM p \a -> let (bs, cs) = genClozePhrase a
                 in return $ genForvos bs is (map f cs)

runPronunciationSpecIO :: Members '[Input Config.ResourceDirs
                                   , FSWrite
                                   , FSExist
                                   , FSRead
                                   , FSDir
                                   ] m
                        => Config.PronunciationSpec
                        -> Sem m [RForvoNote]
runPronunciationSpecIO (Config.PronunciationSpec f ms a) = do
                                                     zs <- forM ms $ runMultiClozeSpecIO f a
                                                     return $ join zs

data Deck = Deck {
  notes :: Map (Path Rel File) Text
, media :: [Path Rel File]
} deriving stock (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup Deck
  deriving Monoid via GenericMonoid Deck

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

extractParts :: Prism' Config.Spec x -> Config.Deck -> Map (Path Rel File) x
extractParts x = Map.fromList . itoListOf
                  ( Config.parts
                  % itraversed
                  %> reindexed (view Config.outfile) selfIndex
                  % Config.spec
                  % x
                  )

type DeckSplit = '[Map (Path Rel File) [Config.MinimalReversedCard]
                 , Map (Path Rel File) [Config.BasicReversedCard]
                 , Map (Path Rel File) [Config.ExcerptSpec]
                 , Map (Path Rel File) [Config.PronunciationSpec]
                 ]

type FileMap b = Map (Path b File)

renderNotes :: RenderNote a => [a] -> Text
renderNotes = T.intercalate "\n" . fmap renderNote

getMedias :: HasMedia a => FileMap Rel [a] -> [Path Rel File]
getMedias = join . Map.elems >=> getMedia

writeOutDeck :: Members '[Input Config.ExportDirs, FSDir, FSWrite] r => Deck -> Sem r ()
writeOutDeck Deck{..} = do
  Config.ExportDirs{..} <- input @Config.ExportDirs
  createDirectory _notes
  forM_ (Map.toList notes) $ \(x, k) ->  writeFileUtf8 (_notes </> x) k

renderDeck :: forall p r a. (HasMedia p, RenderNote p)
           => Sem (Methodology (FileMap Rel [p]) Deck : r) a
           -> Sem r a
renderDeck = divideMethodology'
         >>> fmapMethodology'
         >>> runMethodologyPure renderNotes
         >>> runMethodologyPure getMedias
         >>> runMethodologyPure (uncurry Deck)

data EventType = Analysis | Extract

data Indicator = Start | Complete

data Event (k :: EventType) (t :: Indicator) a = Event a

log_msg_analysing_deck :: Config.Deckname -> Text
log_msg_analysing_deck = F.sformat ("Analysing Deck: " F.% F.stext)

log_msg_deck_analysis_complete :: Config.Deckname -> Text
log_msg_deck_analysis_complete = F.sformat ("Analysis Complete For Deck: " F.% F.stext)

class Display a where
  display :: a -> Text

instance Display (Prism' Config.Spec [Config.MinimalReversedCard]) where
  display = const "Minimal Reversed Card Specs"

instance Display (Prism' Config.Spec [Config.BasicReversedCard]) where
  display = const "Basic Reversed Card Specs"

log_msg_extracting_specs :: Display (Prism' Config.Spec x) => Prism' Config.Spec x -> Text
log_msg_extracting_specs = F.sformat ("Extracting " F.% F.stext) . Main.display

log_msg_spec_extraction_complete :: Display (Prism' Config.Spec x) => Prism' Config.Spec x -> Int -> Text
log_msg_spec_extraction_complete x y = F.sformat ("Found " F.% F.int F.% " " F.% F.stext) y (Main.display x)

msgAnalysisStartDeckConfig :: Event 'Analysis 'Start Config.Deckname -> Text
msgAnalysisStartDeckConfig (Event n) = log_msg_analysing_deck n

msgAnalysisCompleteDeckConfig :: Event 'Analysis 'Complete Config.Deckname -> Text
msgAnalysisCompleteDeckConfig (Event n) = log_msg_deck_analysis_complete n

msgExtractionStartDeckConfig :: Display (Prism' Config.Spec x) => Event 'Extract 'Start (Prism' Config.Spec x) -> Text
msgExtractionStartDeckConfig (Event n) = log_msg_extracting_specs n

msgExtractionCompleteDeckConfig :: Display (Prism' Config.Spec x)
                                => Event 'Extract 'Complete ((Prism' Config.Spec x, FileMap Rel x))
                                -> Text
msgExtractionCompleteDeckConfig (Event (n, c)) = log_msg_spec_extraction_complete n (length c)

main :: IO ()
main = do
  Config.FlashBlast{..} <- D.input D.auto "./index.dhall"
  forM_ (Map.toList _decks) $ \(n, x) -> do
    flashblast @Config.Deck @Deck
      & untag @DeckConfiguration
      & runInputConst x
      & untag @CollectionsPackage
      & runOutputSem writeOutDeck
      & untag @ConstructionMethodology
        & decomposeMethodology @Config.Deck @DeckSplit @Deck
        & logMethodologyAround @Config.Deck @(HList DeckSplit)
            (const $ Event @'Analysis @'Start n)
            (const $ Event @'Analysis @'Complete n)
          & separateMethodologyInitial @Config.Deck @(FileMap Rel [Config.MinimalReversedCard])
            & logMethodologyAround @Config.Deck @(FileMap Rel [Config.MinimalReversedCard])
              (const $ Event @'Extract @'Start Config._MinimalReversed)
              (Event @'Extract @'Complete . (Config._MinimalReversed,))
            & runMethodologyPure (extractParts Config._MinimalReversed)
          & separateMethodologyInitial @Config.Deck @(FileMap Rel [Config.BasicReversedCard])
            & logMethodologyAround @Config.Deck @(FileMap Rel [Config.BasicReversedCard])
              (const $ Event @'Extract @'Start Config._BasicReversed)
              (Event @'Extract @'Complete . (Config._BasicReversed,))
            & runMethodologyPure (extractParts Config._BasicReversed)
          & separateMethodologyInitial @Config.Deck @(FileMap Rel [Config.ExcerptSpec])
            & traceMethodologyAround @Config.Deck @(FileMap Rel [Config.ExcerptSpec])
              (const "Extracting Excerpt Specs")
              (\c -> "Found " <> show (length c) <> " Excerpt specs.")
            & runMethodologyPure (extractParts Config._Excerpt)
          & separateMethodologyInitial @Config.Deck @(FileMap Rel [Config.PronunciationSpec])
            & traceMethodologyAround @Config.Deck @(FileMap Rel [Config.PronunciationSpec])
              (const "Extracting Pronunciation Specs")
              (\c -> "Found " <> show (length c) <> " Pronunciation specs.")
            & runMethodologyPure (extractParts Config._Pronunciation)
          & endMethodologyInitial
          & separateMethodologyTerminal @(FileMap Rel [Config.MinimalReversedCard]) @Deck
            & traceMethodologyStart @(FileMap Rel [Config.MinimalReversedCard]) @Deck
             (const "Constructing Minimal Reversed notes")
            & cutMethodology @(FileMap Rel [Config.MinimalReversedCard])
                             @(FileMap Rel [RMinimalNoteVF])
                             @Deck
            & traceMethodologyEnd @(FileMap Rel [Config.MinimalReversedCard])
                                  @(FileMap Rel [RMinimalNoteVF])
              (\c -> "Produced " <> show (length $ foldr (++) [] c) <> " notes.")
            & fmap2Methodology @(FileMap Rel) @[] @Config.MinimalReversedCard @RMinimalNoteVF
              & runMethodologyPure generateMinimalReversedNoteVF
            & renderDeck @RMinimalNoteVF
          & separateMethodologyTerminal @(FileMap Rel [Config.BasicReversedCard]) @Deck
            & traceMethodologyStart @(FileMap Rel [Config.BasicReversedCard]) @Deck
             (const "Constructing Basic Reversed notes")
            & cutMethodology @(FileMap Rel [Config.BasicReversedCard])
                             @(FileMap Rel [RBasicReversedNoteVF])
                             @Deck
            & traceMethodologyEnd @(FileMap Rel [Config.BasicReversedCard])
                                  @(FileMap Rel [RBasicReversedNoteVF])
              (\c -> "Produced " <> show (length $ foldr (++) [] c) <> " notes.")
              & fmap2Methodology @(FileMap Rel) @[] @Config.BasicReversedCard @RBasicReversedNoteVF
                & runMethodologyPure generateBasicReversedNoteVF
              & renderDeck @RBasicReversedNoteVF
        & separateMethodologyTerminal @(FileMap Rel [Config.ExcerptSpec]) @Deck
            & traceMethodologyStart @(FileMap Rel [Config.ExcerptSpec]) @Deck
             (const "Constructing Excerpt notes")
          & cutMethodology @(FileMap Rel [Config.ExcerptSpec])
                           @(FileMap Rel [RExcerptNote])
                           @Deck
            & traceMethodologyEnd @(FileMap Rel [Config.ExcerptSpec])
                                  @(FileMap Rel [RExcerptNote])
              (\c -> "Produced " <> show (length $ foldr (++) [] c) <> " notes.")
            & fmapMethodology @(FileMap Rel) @[Config.ExcerptSpec] @[RExcerptNote]
              & bindMethodology @[] @Config.ExcerptSpec @RExcerptNote
                & runMethodologySem @Config.ExcerptSpec @[RExcerptNote] runExcerptSpecIO
            & renderDeck @RExcerptNote
        & separateMethodologyTerminal @(Map (Path Rel File) [Config.PronunciationSpec]) @Deck
            & traceMethodologyStart @(FileMap Rel [Config.PronunciationSpec]) @Deck
             (const "Constructing Pronunciation notes")
          & cutMethodology @(FileMap Rel [Config.PronunciationSpec])
                           @(FileMap Rel [RForvoNote])
                           @Deck
            & traceMethodologyEnd @(FileMap Rel [Config.PronunciationSpec])
                                  @(FileMap Rel [RForvoNote])
              (\c -> "Produced " <> show (length $ foldr (++) [] c) <> " notes.")
            & fmapMethodology @(FileMap Rel) @[Config.PronunciationSpec] @[RForvoNote]
              & bindMethodology @[] @Config.PronunciationSpec @RForvoNote
               & runMethodologySem runPronunciationSpecIO
               & renderDeck @RForvoNote
        & endMethodologyTerminal
        & runInputConst @Config.ExportDirs   (view Config.exportDirs x)
        & runInputConst @Config.ResourceDirs (view Config.resourceDirs x)
        & runFSWrite
        & runFSDir
        & runFSCopy
        & runFSExist
        & runFSTemp
        & runFSRead
        & interpretYouTubeDL
        & runError @SubtitleParseException
        & runError @ForvoLimitReachedException
        & runRemoteHttpRequest
        & runError @ForvoResponseNotUnderstood
        & runError @ForvoAPIKeyIncorrectException
        & runError @JSONException
        & runError @BadRequestException
        & interpretFFMpegCli
        & runLogAction (logTextStderr >$$< msgAnalysisStartDeckConfig)
        & runLogAction (logTextStderr >$$< msgAnalysisCompleteDeckConfig)
        & runLogAction (logTextStderr >$$< msgExtractionStartDeckConfig @[Config.MinimalReversedCard])
        & runLogAction (logTextStderr >$$< msgExtractionCompleteDeckConfig @[Config.MinimalReversedCard])
        & runLogAction (logTextStderr >$$< msgExtractionStartDeckConfig @[Config.BasicReversedCard])
        & runLogAction (logTextStderr >$$< msgExtractionCompleteDeckConfig @[Config.BasicReversedCard])
        & traceToIO
        & runM
