{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module FlashBlast.Config where

import qualified Dhall                  as D
import qualified Dhall.Deriving         as D
import           FlashBlast.ForvoClient
import           FlashBlast.VF
import           Path
import           Path.Dhall             ()
import           Optics.TH
import           RIO

data MultiClozeSpec = MultiClozeSpec {
  _phrases :: [Text]
, _images  :: [Path Rel File]
} deriving stock (Eq, Generic, Show)
  deriving (D.FromDhall, D.ToDhall)
    via D.Codec (D.Field (D.DropPrefix "_")) MultiClozeSpec

makeFieldsNoPrefix ''MultiClozeSpec

data YDLInfo = YDLInfo {
  _url    :: Text
, _out    :: Path Rel File
, _format :: Text
} deriving stock (Eq, Generic, Show)
  deriving (D.FromDhall, D.ToDhall)
    via D.Codec (D.Field (D.DropPrefix "_")) YDLInfo

makeFieldsNoPrefix ''YDLInfo

data ResourceDirs = ResourceDirs {
  _audio  :: Path Rel Dir
, _video  :: Path Rel Dir
, _images :: Path Rel Dir
} deriving stock (Eq, Generic, Show)
  deriving (D.FromDhall, D.ToDhall)
    via D.Codec (D.Field (D.DropPrefix "_")) ResourceDirs

makeFieldsNoPrefix ''ResourceDirs

data VideoSource = LocalVideo (Path Rel File) | YouTubeDL YDLInfo
  deriving stock (Eq, Generic, Show)
  deriving (D.FromDhall, D.ToDhall)
    via D.Codec (D.Field (D.DropPrefix "_")) VideoSource

makePrisms ''VideoSource

data ExcerptSpec = ExcerptSpec {
  _source :: VideoSource
, _subs   :: Text
, _clipf  :: Text -> Path Rel File
, _audiof :: Text -> Path Rel File
, _framef :: Text -> Path Rel File
} deriving stock Generic
  deriving D.FromDhall
    via D.Codec (D.Field (D.DropPrefix "_")) ExcerptSpec

makeFieldsNoPrefix ''ExcerptSpec

data ExportDirs = ExportDirs {
  _audio  :: Path Rel Dir
, _clips  :: Path Rel Dir
, _images :: Path Rel Dir
, _notes  :: Path Rel Dir
} deriving stock (Eq, Generic, Show)
  deriving (D.FromDhall, D.ToDhall)
    via D.Codec (D.Field (D.DropPrefix "_")) ExportDirs

makeFieldsNoPrefix ''ExportDirs

newtype Speaker = Speaker Text
  deriving (Eq, Show, Generic)

instance D.FromDhall Locale
instance D.ToDhall Locale

instance D.FromDhall Speaker
instance D.ToDhall Speaker

data ForvoSpec = ForvoSpec {
  _locale            :: Locale
} deriving stock (Eq, Generic, Show)
  deriving (D.FromDhall, D.ToDhall)
    via D.Codec (D.Field (D.DropPrefix "_")) ForvoSpec

makeFieldsNoPrefix ''ForvoSpec

data PronunciationSpec = PronunciationSpec {
  _audiof  :: Text -> Path Rel File
, _multis :: [MultiClozeSpec]
, _forvo  :: Maybe ForvoSpec
} deriving stock Generic
  deriving D.FromDhall
    via D.Codec (D.Field (D.DropPrefix "_")) PronunciationSpec

makeFieldsNoPrefix ''PronunciationSpec

data BasicReversedCard = BasicReversedCard {
  _front       :: VF
, _front_extra :: VFC
, _back        :: VF
, _back_extra  :: VFC
} deriving stock (Eq, Generic, Show)
  deriving (D.FromDhall, D.ToDhall)
    via D.Codec (D.Field (D.DropPrefix "_")) BasicReversedCard

makeFieldsNoPrefix ''BasicReversedCard

data MinimalReversedCard = MinimalReversedCard {
  _front :: VF
, _back  :: VF
} deriving stock (Eq, Show, Generic)
  deriving (D.FromDhall, D.ToDhall)
    via D.Codec (D.Field (D.DropPrefix "_")) MinimalReversedCard

makeFieldsNoPrefix ''MinimalReversedCard

data Spec =
    Pronunciation   [PronunciationSpec]
  | Excerpt         [ExcerptSpec]
  | BasicReversed   [BasicReversedCard]
  | MinimalReversed [MinimalReversedCard]
    deriving stock Generic
    deriving D.FromDhall
      via D.Codec (D.Field (D.DropPrefix "_")) Spec

makePrisms ''Spec

instance D.FromDhall ForvoAPIKey

data Part = Part {
  _outfile :: Path Rel File
, _spec    :: Spec
} deriving Generic
  deriving D.FromDhall
    via D.Codec (D.Field (D.DropPrefix "_")) Part

makeFieldsNoPrefix ''Part

data Deck = Deck {
  _resourceDirs :: ResourceDirs
, _exportDirs   :: ExportDirs
, _parts        :: [Part]
} deriving stock Generic
  deriving D.FromDhall
    via D.Codec (D.Field (D.DropPrefix "_")) Deck

makeFieldsNoPrefix ''Deck

type Deckname = Text

data FlashBlast = FlashBlast {
  _decks       :: Map Deckname Deck
, _forvoApiKey :: Maybe ForvoAPIKey
} deriving stock Generic
  deriving D.FromDhall
    via D.Codec (D.Field (D.DropPrefix "_")) FlashBlast

makeFieldsNoPrefix ''FlashBlast
