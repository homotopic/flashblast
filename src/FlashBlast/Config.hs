{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module FlashBlast.Config where

import qualified Dhall                  as D
import           FlashBlast.Conventions
import           FlashBlast.ForvoClient
import           Path
import           Path.Dhall             ()
import           Optics.TH
import           RIO

data MultiClozeSpec = MultiClozeSpec {
  _phrases :: [Text]
, _images  :: [Path Rel File]
} deriving (Eq, Generic, Show)

makeFieldsNoPrefix ''MultiClozeSpec

instance D.FromDhall MultiClozeSpec

data YDLInfo = YDLInfo {
  _url    :: Text
, _out    :: Path Rel File
, _format :: Text
} deriving (Eq, Generic, Show)

makeFieldsNoPrefix ''YDLInfo

instance D.FromDhall YDLInfo

data ResourceDirs = ResourceDirs {
  _audio  :: Path Rel Dir
, _video  :: Path Rel Dir
, _images :: Path Rel Dir
} deriving (Eq, Show, Generic)

makeFieldsNoPrefix ''ResourceDirs

instance D.FromDhall ResourceDirs

data VideoSource = LocalVideo (Path Rel File) | YouTubeDL YDLInfo
  deriving (Eq, Generic, Show)

makePrisms ''VideoSource

instance D.FromDhall VideoSource

data ExcerptSpec = ExcerptSpec {
  _source :: VideoSource
, _subs   :: Text
, _clipf  :: Text -> Path Rel File
, _audiof :: Text -> Path Rel File
, _framef :: Text -> Path Rel File
} deriving Generic

makeFieldsNoPrefix ''ExcerptSpec

instance D.FromDhall ExcerptSpec

data ExportDirs = ExportDirs {
  _audio  :: Path Rel Dir
, _clips  :: Path Rel Dir
, _images :: Path Rel Dir
, _notes  :: Path Rel Dir
} deriving (Eq, Show, Generic)

makeFieldsNoPrefix ''ExportDirs

instance D.FromDhall ExportDirs

newtype Speaker = Speaker Text
  deriving (Eq, Show, Generic)

instance D.FromDhall Speaker

data ForvoSpec = ForvoSpec {
  _locale            :: Locale
, _preferredspeakers :: [Speaker]
} deriving (Eq, Show, Generic)

instance D.FromDhall ForvoSpec

makeFieldsNoPrefix ''ForvoSpec

data PronunciationSpec = PronunciationSpec {
  _audiof  :: Text -> Path Rel File
, _multis :: [MultiClozeSpec]
, _forvo  :: Maybe ForvoSpec
} deriving Generic

makeFieldsNoPrefix ''PronunciationSpec

instance D.FromDhall PronunciationSpec

data BasicReversedCard = BasicReversedCard {
  _front       :: VF
, _front_extra :: VF
, _back        :: VF
, _back_extra  :: VF
} deriving (Eq, Show, Generic)

instance D.FromDhall BasicReversedCard

makeFieldsNoPrefix ''BasicReversedCard

instance D.FromDhall Locale

data MinimalReversedCard = MinimalReversedCard {
  _front :: VF
, _back  :: VF
} deriving (Eq, Show, Generic)

makeFieldsNoPrefix ''MinimalReversedCard

instance D.FromDhall MinimalReversedCard

data Spec =
    Pronunciation   PronunciationSpec
  | Excerpt         [ExcerptSpec]
  | BasicReversed   [BasicReversedCard]
  | MinimalReversed [MinimalReversedCard]
    deriving Generic

makePrisms ''Spec

instance D.FromDhall Spec

instance D.FromDhall ForvoAPIKey

data Part = Part {
  _outfile :: Path Rel File
, _spec    :: Spec
} deriving Generic

makeFieldsNoPrefix ''Part

instance D.FromDhall Part

data Deck = Deck {
  _resourceDirs :: ResourceDirs
, _exportDirs   :: ExportDirs
, _parts        :: [Part]
} deriving Generic

makeFieldsNoPrefix ''Deck

instance D.FromDhall Deck

data FlashBlast = FlashBlast {
  _decks :: Map Text Deck
, _forvoApiKey :: Maybe ForvoAPIKey
} deriving Generic

makeFieldsNoPrefix ''FlashBlast

instance D.FromDhall FlashBlast
