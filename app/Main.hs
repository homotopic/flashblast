{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

import Colog.Polysemy
import Colog.Polysemy.Formatting
import Composite.CoRecord
import Composite.Record
import Control.Comonad.Env
import Control.Monad.Extra hiding (whenM)
import Data.Kind
import Data.Monoid.Generic
import Data.Vinyl
import Data.Vinyl.Functor hiding (Identity)
import qualified Dhall as D
import Fcf hiding (Error, Map)
import Fcf.Class.Functor hiding (Map)
import FlashBlast.ClozeParse
import qualified FlashBlast.Config as Config
import FlashBlast.Conventions
import FlashBlast.Domain
import FlashBlast.ForvoClient hiding (id)
import FlashBlast.Messages
import FlashBlast.VF
import FlashBlast.YouTubeDL
import qualified Formatting as F
import Media.Subtitles.SRT
import Media.Timestamp
import Optics
import Path
import Path.Dhall ()
import Path.Utils
import Polysemy
import Polysemy.Error as P
import Polysemy.FS
import Polysemy.FSKVStore
import Polysemy.Http hiding (Path)
import Polysemy.Input
import Polysemy.KVStore
import Polysemy.Log
import Polysemy.Methodology
import Polysemy.Methodology.Colog
import Polysemy.Methodology.Composite
import Polysemy.Output
import Polysemy.Resource
import Polysemy.Tagged
import Polysemy.Video
import Polysemy.Vinyl
import RIO hiding
  ( Builder,
    Display,
    log,
    logInfo,
    over,
    to,
    trace,
    view,
    writeFileUtf8,
    (^.),
  )
import RIO.List
import qualified RIO.Map as Map
import qualified RIO.Text as T

interpretVideoSource ::
  Members '[Input Config.ResourceDirs, YouTubeDL] m =>
  Config.VideoSource ->
  Sem m (Path Rel File)
interpretVideoSource = \case
  Config.YouTubeDL (Config.YDLInfo x y f) -> do
    Config.ResourceDirs {..} <- input @Config.ResourceDirs
    youTubeDL' x (_video </> y) f
    return (_video </> y)
  Config.LocalVideo x -> do
    Config.ResourceDirs {..} <- input @Config.ResourceDirs
    return (_video </> x)

runExcerptSpecIO ::
  Members
    '[ FSExist,
       FSTemp,
       FSCopy,
       FSDir,
       Input Config.ExportDirs,
       Input Config.ResourceDirs,
       YouTubeDL,
       ClipProcess
     ]
    m =>
  Config.ExcerptSpec ->
  Sem m [RExcerptNote]
runExcerptSpecIO Config.ExcerptSpec {..} = do
  Config.ExportDirs {..} <- input @Config.ExportDirs
  t <- interpretVideoSource _source
  let (SRT s') = _subs
  let cs = map (_clipf . T.pack . show . index) s'
  let es = map (_audiof . T.pack . show . index) s'
  let fs = map (_framef . T.pack . show . index) s'
  cs' <- filterM (fmap not . doesFileExist . (_clips </>)) cs
  es' <- filterM (fmap not . doesFileExist . (_audio </>)) es
  h <- createTempDirectory
  createDirectory _clips
  createDirectory _audio
  createDirectory _images
  unless (null cs') $ do
    extractClips t $ zip (range <$> s') (h </$> cs')
    forM_ cs' $ \x -> copyFile (h </> x) (_clips </> x)
  unless (null es') $ do
    extractAudio t $ zip (range <$> s') (h </$> es)
    forM_ es' $ \x -> copyFile (h </> x) (_audio </> x)
  removeDirectory h
  forM (zip4 s' cs es fs) $ \(l, c, e, f) -> do
    whenM (fmap not . doesFileExist $ _images </> f) $
      extractFrames (_clips </> c) [(Time 0 0 0 0, _images </> f)]
    return $
      val @"front" (RawText (fst . genClozePhrase . dialog $ l))
        :& val @"extra" (VFC [Image f])
        :& val @"back" (Audio e)
        :& RNil

data Meth :: Type -> Type -> Type where
  MkMeth :: Meth a b

data Cut :: Meth a c -> Type -> Exp (Meth a b, Meth b c)

type family ToM (x :: Exp (Meth a b)) (r :: EffectRow) :: Constraint

data (>>=) :: Exp a -> (a -> Exp b) -> Exp b

type A = MkMeth :: Meth Int String

type X = (Cut A Bool >>= Fst) :: Exp (Meth Int Bool)

type family ToMethodology a (r :: EffectRow) :: Constraint

type instance ToMethodology (Meth a b) r = Member (Methodology a b) r

type instance ToM (X :: Exp (Meth Int Bool)) r = Member (Methodology Int Bool) r

data FreeCategory :: Type -> Type -> Type

data ID :: FreeCategory x x -> Type

data Comp :: FreeCategory x y -> FreeCategory y z -> FreeCategory x z -> Type

{--
type Op x y = CoRec Identity '[ID, Comp]
data FreeCategory = IDA | CompA
type T = CoRec FCK '[IDA, CompA]
--}

data FCK :: FreeCategory a b -> Type

{--
data FreeCategory x y where
  ID :: FreeCategory x x
  Comp :: FreeCategory x y -> FreeCategory y z -> FreeCategory x z
--}
data FreeGADT :: Type -> Type where
  GCSTR :: Composite.Record.RElem a x => a -> FreeGADT (Record x)

downloadMP3For :: Members '[ForvoClient] r => Locale -> Text -> Sem r (Maybe ByteString)
downloadMP3For l t = do
  ForvoStandardPronunciationResponseBody {..} <- standardPronunciation l t
  traverse mP3For . headMaybe $ items

getForvo ::
  Members
    '[ KVStore (Path Rel File) ByteString,
       ForvoClient
     ]
    r =>
  Locale ->
  Text ->
  Path Rel File ->
  Sem r ()
getForvo l t f = do
  z <- lookupKV f
  case z of
    Just _ -> return ()
    Nothing -> downloadMP3For l t >>= updateKV f

runMultiClozeSpecIO ::
  Members
    '[ Input Config.ResourceDirs,
       KVStore (Path Rel File) ByteString,
       Http (IO ByteString),
       Error HttpError,
       Error SomeException
     ]
    m =>
  (Text -> Path Rel File) ->
  Maybe ForvoAPIKey ->
  Maybe Config.ForvoSpec ->
  Config.MultiClozeSpec ->
  Sem m [RPronunciationNote]
runMultiClozeSpecIO f y s (Config.MultiClozeSpec p is) = do
  forM p \a -> do
    let (bs, cs) = genClozePhrase a
    Config.ResourceDirs {..} <- input
    P.catch
      ( whenJust (liftA2 (,) s y) \(Config.ForvoSpec l, k) ->
          forM_ cs $ \x ->
            getForvo l x (_audio </> f x)
              & runForvoClient
              & mapError @ForvoResponseNotUnderstood SomeException
              & mapError @ForvoLimitReachedException SomeException
              & mapError @ForvoAPIKeyIncorrectException SomeException
              & runInputConst @ForvoAPIKey k
      )
      (\(_ :: SomeException) -> return ())
    return $ genForvos bs (VFC $ map Image is) (map (Audio . f) cs)

runPronunciationSpecIO ::
  Members
    '[ KVStore (Path Rel File) ByteString,
       Error HttpError,
       Input Config.ResourceDirs,
       Http (IO ByteString),
       Error SomeException
     ]
    m =>
  Maybe ForvoAPIKey ->
  Config.PronunciationSpec ->
  Sem m [RPronunciationNote]
runPronunciationSpecIO k (Config.PronunciationSpec f ms a) =
  fmap join $ forM ms $ runMultiClozeSpecIO f k a

data Deck = Deck
  { notes :: Map (Path Rel File) Text,
    media :: [Path Rel File]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Semigroup) via GenericSemigroup Deck
  deriving (Monoid) via GenericMonoid Deck

generateMinimalReversedNoteVF :: Config.MinimalReversedCard -> RMinimalNote
generateMinimalReversedNoteVF Config.MinimalReversedCard {..} =
  val @"front" _front
    :& val @"back" _back
    :& RNil

generateBasicReversed :: Config.BasicReversedCard -> RBasicNote
generateBasicReversed Config.BasicReversedCard {..} =
  val @"front" _front
    :& val @"front-extra" _front_extra
    :& val @"back" _back
    :& val @"back-extra" _back_extra
    :& RNil

extractParts :: Prism' Config.Spec x -> Config.Deck -> Map (Path Rel File) x
extractParts x =
  Map.fromList
    . itoListOf
      ( Config.parts
          % itraversed
          %> reindexed (view Config.outfile) selfIndex
          % Config.spec
          % x
      )

type FileMap b = Map (Path b File)

renderNotes ::
  ( RecMapMethod RenderVF Identity xs,
    RecordToList xs
  ) =>
  [Record xs] ->
  Text
renderNotes = T.intercalate "\n" . fmap renderNote

writeOutDeck :: Members '[Input Config.ExportDirs, FSDir, FSWrite] r => Deck -> Sem r ()
writeOutDeck Deck {..} = do
  Config.ExportDirs {..} <- input @Config.ExportDirs
  createDirectory _notes
  forM_ (Map.toList notes) $ \(x, k) -> writeFileUtf8 (_notes </> x) k

renderDeck :: (RecMapMethod RenderVF Identity xs, RecordToList xs, RecMapMethod (HasMedia (Path Rel File)) Identity xs) => Path Rel File -> [Record xs] -> Deck
renderDeck x as = Deck [(x, renderNotes as)] (getRecordMedia =<< as)

type CardTypes = ['Minimal, 'Basic, 'Excerpt, 'Pronunciation]

type instance ConfigFor 'Minimal = Config.MinimalReversedCard

type instance ConfigFor 'Basic = Config.BasicReversedCard

type instance ConfigFor 'Excerpt = Config.ExcerptSpec

type instance ConfigFor 'Pronunciation = Config.PronunciationSpec

type instance ResultFor 'Minimal = RMinimalNote

type instance ResultFor 'Basic = RBasicNote

type instance ResultFor 'Excerpt = RExcerptNote

type instance ResultFor 'Pronunciation = RPronunciationNote

type PartTypes = Eval (FMap ConfigFor' CardTypes)

type NoteTypes = Eval (FMap ResultFor' CardTypes)

type AnalysisF = FileMap Rel :. []

type StagingF = [] :. Env (Path Rel File) :. []

type DiffractF = Env (Path Rel File) :. []

reduceStaging ::
  forall (c :: CardType) r a.
  ResultFor c ∈ NoteTypes =>
  Sem (Methodology (StagingF (ConfigFor c)) [CoRec DiffractF NoteTypes] ': r) a ->
  Sem (Methodology (DiffractF (ConfigFor c)) (DiffractF (ResultFor c)) ': r) a
reduceStaging =
  cutMethodology' @(StagingF (ConfigFor c))
    @[DiffractF (ConfigFor c)]
    >>> runMethodologyPure getCompose
    >>> fmapMethodology'
    >>> pickCoRecConstructor @(ResultFor c)

main :: IO ()
main = do
  Config.FlashBlast {..} <- D.input D.auto "./index.dhall"
  forM_ (Map.toList _decks) $ \(n, x) -> do
    flashblast @Config.Deck @Deck
      & untag @DeckConfiguration
      & runInputConst x
      & untag @CollectionsPackage
      & runOutputSem writeOutDeck
      & untag @ConstructionMethodology
      & logMethodologyAround @Config.Deck @Deck
        (const $ F.sformat msgBuildingDeck n)
        (const $ F.sformat msgDeckComplete n)
      & diffractMethodology' @Config.Deck @DiffractF @NoteTypes @Deck
      & cutMethodology' @Config.Deck @(Rec AnalysisF PartTypes)
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
      & runFSKVStoreRelBS $(mkRelDir ".")
      & runInputConst @Config.ExportDirs (view Config.exportDirs x)
      & runInputConst @Config.ResourceDirs (view Config.resourceDirs x)
      & runFSDir
      & runFSCopy
      & runFSTemp
      & runFSExist
      & runFSRead
      & runFSWrite
      & interpretYouTubeDL
      & runFFMpegCli
      & interpretHttpNative
      & runError @SomeException
      & runError @HttpError
      & runLogAction logTextStderr
      & interpretLogNull
      & resourceToIO
      & runM
