{-# LANGUAGE DuplicateRecordFields #-}

import           Colog.Polysemy
import           Colog.Polysemy.Formatting
import           Colog.Polysemy.Formatting.Color
import           Colog.Polysemy.Formatting.LogEnv
import           Colog.Polysemy.Formatting.Render
import           Colog.Polysemy.Formatting.ThreadTimeMessage
import           Composite.Record
import qualified Data.Attoparsec.Text                        as A
import Fcf hiding (Map, Error)
import Fcf.Class.Functor hiding (Map)
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
import           Polysemy.Video                              hiding (to)
import           RIO.Time
import Polysemy.FSKVStore

import           FlashBlast.ClozeParse
import Control.Comonad.Env
import qualified FlashBlast.Config                           as Config
import Polysemy.Methodology.Composite
import Data.Kind
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
import Data.Vinyl.Functor
import Polysemy.Vinyl
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

type FileMap b = Map (Path b File)

renderNotes :: RenderNote a => [a] -> Text
renderNotes = T.intercalate "\n" . fmap renderNote

writeOutDeck :: Members '[Input Config.ExportDirs, FSDir, FSWrite] r => Deck -> Sem r ()
writeOutDeck Deck{..} = do
  Config.ExportDirs{..} <- input @Config.ExportDirs
  createDirectory _notes
  forM_ (Map.toList notes) $ \(x, k) ->  writeFileUtf8 (_notes </> x) k

renderDeck :: forall p. (HasMedia p, RenderNote p) => Path Rel File -> [p] -> Deck
renderDeck x as = Deck [(x, renderNotes as)] (join $ fmap getMedia as)

type CardTypes = ['Minimal, 'Basic, 'Excerpt, 'Pronunciation]

type instance ConfigFor 'Minimal       = Config.MinimalReversedCard
type instance ConfigFor 'Basic         = Config.BasicReversedCard
type instance ConfigFor 'Excerpt       = Config.ExcerptSpec
type instance ConfigFor 'Pronunciation = Config.PronunciationSpec

type instance ResultFor 'Minimal       = RMinimalNoteVF
type instance ResultFor 'Basic         = RBasicReversedNoteVF
type instance ResultFor 'Excerpt       = RExcerptNote
type instance ResultFor 'Pronunciation = RForvoNote

type PartTypes = Eval (FMap ConfigFor' CardTypes)
type NoteTypes = Eval (FMap ResultFor' CardTypes)

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
      & diffractMethodology'  @Config.Deck @(Env (Path Rel File) :. []) @NoteTypes @Deck
      & cutMethodology'       @Config.Deck @(Rec (FileMap Rel :. []) PartTypes)
      & runRecInitialAsInputCompose'
      & stripRecInput
      & stripRecInput
      & stripRecInput
      & stripRecInput
      & endRecInput
      & cutMethodology' @(Rec (FileMap Rel :. []) PartTypes)
                        @(Rec ([] :. (Env (Path Rel File)) :. []) PartTypes)
      & runMethodologyRmap (onCompose (fmap Compose . fmap (uncurry env) . Map.toList))
      & stripRecTerminal
      & stripRecTerminal
      & stripRecTerminal
      & stripRecTerminal
      & endRecTerminal
      & stripRecInput
      & stripRecInput
      & stripRecInput
      & stripRecInput
      & endRecInput
      & runInputCase' @RMinimalNoteVF
        (uncurry renderDeck . runEnv . getCompose)
      & runInputCase' @RBasicReversedNoteVF
        (uncurry renderDeck . runEnv . getCompose)
      & runInputCase' @RExcerptNote
        (uncurry renderDeck . runEnv . getCompose)
      & runInputCase' @RForvoNote
        (uncurry renderDeck . runEnv . getCompose)
      & subsume
      & runInputConstFC @Config.MinimalReversedCard
         (Compose . extractParts Config._MinimalReversed)
      & runInputConstFC @Config.BasicReversedCard
         (Compose . extractParts Config._BasicReversed)
      & runInputConstFC @Config.ExcerptSpec
         (Compose . extractParts Config._Excerpt)
      & runInputConstFC @Config.PronunciationSpec
         (Compose . extractParts Config._Pronunciation)
      & cutMethodology' @(([] :. Env (Path Rel File) :. []) Config.MinimalReversedCard)
                       @[((Env (Path Rel File) :. []) Config.MinimalReversedCard)]
      & runMethodologyPure getCompose
      & fmapMethodology'
      & pickCoRecConstructor @RMinimalNoteVF
      & fmapMethodology' @(Env (Path Rel File) :. [])
      & runMethodologyPure generateMinimalReversedNoteVF
      & cutMethodology' @(([] :. Env (Path Rel File) :. []) Config.BasicReversedCard)
                       @[((Env (Path Rel File) :. []) Config.BasicReversedCard)]
      & runMethodologyPure getCompose
      & fmapMethodology'
      & pickCoRecConstructor @RBasicReversedNoteVF
      & fmapMethodology' @(Env (Path Rel File) :. [])
      & runMethodologyPure generateBasicReversedNoteVF
      & cutMethodology' @(([] :. Env (Path Rel File) :. []) Config.ExcerptSpec)
                       @[((Env (Path Rel File) :. []) Config.ExcerptSpec)]
      & runMethodologyPure getCompose
      & fmapMethodology'
      & pickCoRecConstructor @RExcerptNote
      & fmapCMethodology
      & bindMethodology'
      & runMethodologySem runExcerptSpecIO
      & cutMethodology' @(([] :. Env (Path Rel File) :. []) Config.PronunciationSpec)
                       @[((Env (Path Rel File) :. []) Config.PronunciationSpec)]
      & runMethodologyPure getCompose
      & fmapMethodology'
      & pickCoRecConstructor @RForvoNote
      & fmapCMethodology
      & bindMethodology'
      & runMethodologySem runPronunciationSpecIO
      & runInputConst @Config.ExportDirs   (view Config.exportDirs x)
      & runInputConst @Config.ResourceDirs (view Config.resourceDirs x)
      & runFSDir
      & runFSCopy
      & runFSTemp
      & runFSExist
      & runFSRead
      & runFSWrite
      & interpretYouTubeDL
      & runError @SubtitleParseException
      & runError @ForvoLimitReachedException
      & runRemoteHttpRequest
      & runError @ForvoResponseNotUnderstood
      & runError @ForvoAPIKeyIncorrectException
      & runError @JSONException
      & runError @BadRequestException
      & interpretFFMpegCli
      & runM
