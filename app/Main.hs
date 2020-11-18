{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

import           Colog.Polysemy
import           Composite.Record
import           Fcf hiding (Map, Error)
import           Fcf.Class.Functor hiding (Map)
import           Data.Monoid.Generic
import qualified Dhall                                       as D
import           Path
import           Path.Dhall                                  ()
import           Path.Utils
import           Polysemy
import           Polysemy.Error                              as P
import           Polysemy.Input
import Polysemy.Resource
import           Polysemy.KVStore
import           Polysemy.Output
import           Polysemy.Video                              hiding (to)
import Polysemy.FSKVStore

import           FlashBlast.ClozeParse
import Control.Comonad.Env
import qualified FlashBlast.Config                           as Config
import Polysemy.Methodology.Composite
import           FlashBlast.Conventions
import           FlashBlast.Domain
import           FlashBlast.ForvoClient                      hiding (id)
import           FlashBlast.YouTubeDL
import Polysemy.FS
import Composite.CoRecord
import           Optics
import           Polysemy.Methodology
import           Polysemy.Tagged
import Polysemy.Http hiding (Path)
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
import Data.Vinyl
import FlashBlast.VF
import FlashBlast.Subtitles
import Control.Monad.Extra hiding (whenM)

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

runExcerptSpecIO :: Members '[ FSExist
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
  let (SRT s') = _subs
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
    return $ val @"front" (RawText (fst . genClozePhrase . SR.dialog $ l))
          :& val @"extra" (Multi [Image f])
          :& val @"back"  (Audio e)
          :& RNil

downloadMP3For :: Members '[ForvoClient] r => Locale -> Text -> Sem r (Maybe ByteString)
downloadMP3For l t = do
  ForvoStandardPronunciationResponseBody {..} <- standardPronunciation l t
  traverse mP3For . headMaybe $ items

getForvo :: Members '[ FSKVStore Rel ByteString
                     , ForvoClient] r
         => Locale -> Text -> Path Rel File -> Sem r ()
getForvo l t f = do
  z <- lookupKV f
  case z of
    Just _ -> return ()
    Nothing -> downloadMP3For l t >>= updateKV f

runMultiClozeSpecIO :: Members '[ Input Config.ResourceDirs
                                , FSKVStore Rel ByteString
                                , Http (IO ByteString)
                                , Error HttpError
                                , Error SomeException
                                ] m
                    => (Text -> Path Rel File)
                    -> Maybe ForvoAPIKey
                    -> Maybe Config.ForvoSpec
                    -> Config.MultiClozeSpec
                    -> Sem m [RPronunciationNote]
runMultiClozeSpecIO f y s (Config.MultiClozeSpec p is) = do
    forM p \a -> do
                   let (bs, cs) = genClozePhrase a
                   Config.ResourceDirs {..} <- input
                   whenJust (liftA2 (,) s y) \(Config.ForvoSpec l, k) ->
                     forM_ cs $ \x -> getForvo l x (_audio </> f x)
                       & runForvoClient
                       & mapError @ForvoResponseNotUnderstood SomeException
                       & mapError @ForvoLimitReachedException SomeException
                       & mapError @ForvoAPIKeyIncorrectException SomeException
                       & runInputConst @ForvoAPIKey k
                   return $ genForvos bs (Multi $ map Image is) (map (Audio . f) cs)

runPronunciationSpecIO :: Members '[FSKVStore Rel ByteString, Error HttpError, Input Config.ResourceDirs, Http (IO ByteString), Error SomeException] m
                       => Maybe ForvoAPIKey
                        -> Config.PronunciationSpec
                        -> Sem m [RPronunciationNote]
runPronunciationSpecIO k (Config.PronunciationSpec f ms a) =
  fmap join $ forM ms $ runMultiClozeSpecIO f k a

data Deck = Deck {
  notes :: Map (Path Rel File) Text
, media :: [Path Rel File]
} deriving stock (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup Deck
  deriving Monoid via GenericMonoid Deck

generateMinimalReversedNoteVF :: Config.MinimalReversedCard -> RMinimalNote
generateMinimalReversedNoteVF Config.MinimalReversedCard{..} = val @"front" _front
                                                            :& val @"back"  _back
                                                            :& RNil

generateBasicReversed :: Config.BasicReversedCard -> RBasicNote
generateBasicReversed Config.BasicReversedCard{..} = val @"front"       _front
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
renderDeck x as = Deck [(x, renderNotes as)] (getMedia =<< as)

type CardTypes = ['Minimal, 'Basic, 'Excerpt, 'Pronunciation]

type instance ConfigFor 'Minimal       = Config.MinimalReversedCard
type instance ConfigFor 'Basic         = Config.BasicReversedCard
type instance ConfigFor 'Excerpt       = Config.ExcerptSpec
type instance ConfigFor 'Pronunciation = Config.PronunciationSpec

type instance ResultFor 'Minimal       = RMinimalNote
type instance ResultFor 'Basic         = RBasicNote
type instance ResultFor 'Excerpt       = RExcerptNote
type instance ResultFor 'Pronunciation = RPronunciationNote

type PartTypes = Eval (FMap ConfigFor' CardTypes)
type NoteTypes = Eval (FMap ResultFor' CardTypes)

type AnalysisF = FileMap Rel :. []
type StagingF  = [] :. Env (Path Rel File) :. []
type DiffractF = Env (Path Rel File) :. []

reduceStaging :: forall (c :: CardType) r a. ResultFor c ∈ NoteTypes
              => Sem (Methodology (StagingF (ConfigFor c)) [CoRec DiffractF NoteTypes] ': r) a
              -> Sem (Methodology (DiffractF (ConfigFor c)) (DiffractF (ResultFor c)) ': r) a
reduceStaging = cutMethodology' @(StagingF (ConfigFor c))
                               @[DiffractF (ConfigFor c)]
            >>> runMethodologyPure getCompose
            >>> fmapMethodology'
            >>> pickCoRecConstructor @(ResultFor c)

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
      & diffractMethodology'  @Config.Deck @DiffractF @NoteTypes @Deck
      & cutMethodology'       @Config.Deck @(Rec AnalysisF PartTypes)
      & runRecInitialAsInputCompose'
      & stripRecInput
      & stripRecInput
      & stripRecInput
      & stripRecInput
      & endRecInput
      & cutMethodology' @(Rec AnalysisF PartTypes)
                        @(Rec StagingF PartTypes)
      & runMethodologyRmap (onCompose (fmap (Compose . uncurry env) . Map.toList))
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
      & runInputCase' @RMinimalNote
        (uncurry renderDeck . runEnv . getCompose)
      & runInputCase' @RBasicNote
        (uncurry renderDeck . runEnv . getCompose)
      & runInputCase' @RExcerptNote
        (uncurry renderDeck . runEnv . getCompose)
      & runInputCase' @RPronunciationNote
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

      & reduceStaging @'Minimal
        & fmapMethodology' @DiffractF
        & runMethodologyPure generateMinimalReversedNoteVF

      & reduceStaging @'Basic
      & fmapMethodology' @DiffractF
      & runMethodologyPure generateBasicReversed

      & reduceStaging @'Excerpt
      & fmapCMethodology
      & bindMethodology'
      & runMethodologySem runExcerptSpecIO

      & reduceStaging @'Pronunciation
      & fmapCMethodology
      & bindMethodology'
      & runMethodologySem (runPronunciationSpecIO _forvoApiKey)

      & runInputConst @Config.ExportDirs   (view Config.exportDirs x)
      & runInputConst @Config.ResourceDirs (view Config.resourceDirs x)
      & runFSDir
      & runFSCopy
      & runFSTemp
      & runFSExist
      & runFSRead
      & runFSWrite
      & interpretYouTubeDL
      & interpretFFMpegCli
      & interpretHttpNative
      & runFSKVStoreRelBS $(mkRelDir ".")
      & runError @SomeException
      & runError @HttpError
      & interpretLogNull
      & resourceToIO
      & runM
