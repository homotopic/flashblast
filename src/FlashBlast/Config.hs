{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module FlashBlast.Config where

import qualified Dhall                as D
import           Path
import           Path.Dhall           ()
import           RIO
import           FlashBlast.Conventions
import FlashBlast.ForvoClient


data MultiClozeSpec = MultiClozeSpec {
  phrases :: [Text]
, images  :: [Path Rel File]
} deriving (Eq, Generic, Show)

instance D.FromDhall MultiClozeSpec

data YDLInfo = YDLInfo {
  url :: Text
, out :: Path Rel File
, format :: Text
} deriving (Eq, Generic, Show)

instance D.FromDhall YDLInfo

data ResourceDirs = ResourceDirs {
  audio  :: Path Rel Dir
, video  :: Path Rel Dir
, images :: Path Rel Dir
} deriving (Eq, Show, Generic)

instance D.FromDhall ResourceDirs

data VideoSource = LocalVideo (Path Rel File) | YouTubeDL YDLInfo
  deriving (Eq, Generic, Show)

instance D.FromDhall VideoSource

data ExcerptSpec = ExcerptSpec {
  source :: VideoSource
, subs  :: Text
, clipf :: Text -> Text
, audiof :: Text -> Text
, framef :: Text -> Text
} deriving Generic

instance D.FromDhall ExcerptSpec

data ExportDirs = ExportDirs {
  audio  :: Path Rel Dir
, clips  :: Path Rel Dir
, images :: Path Rel Dir
, notes  :: Path Rel Dir
} deriving (Eq, Show, Generic)

instance D.FromDhall ExportDirs

instance D.FromDhall ForvoAPIKey

data Deck = Deck {
  resourceDirs :: ResourceDirs
, exportDirs   :: ExportDirs
, parts        :: [Part]
} deriving Generic

data Part = Part {
  outfile :: Path Rel File
, spec   :: Spec
} deriving Generic

instance D.FromDhall Deck

instance D.FromDhall Part

data BasicReversedSpec = BasicReversedSpec {
  from       :: VF
, from_extra :: VF
, to         :: VF
, to_extra   :: VF
} deriving (Eq, Show, Generic)

instance D.FromDhall BasicReversedSpec

instance D.FromDhall Locale

data ForvoSpec = ForvoSpec {
  locale :: Locale
, apiKey :: ForvoAPIKey
} deriving (Eq, Show, Generic)

instance D.FromDhall ForvoSpec

data PronunciationSpec = PronunciationSpec {
 audiof :: Text -> Path Rel File
, multis :: [MultiClozeSpec]
, forvo :: Maybe ForvoSpec
} deriving Generic

instance D.FromDhall PronunciationSpec
data Spec =
    Pronunciation PronunciationSpec
  | Excerpt [ExcerptSpec]
  | BasicReversed [BasicReversedSpec]
  | MinimalReversed [MinimalReversedSpec]
    deriving Generic

instance D.FromDhall Spec

data FlashBlastConfig = FlashBlastConfig {
  decks :: Map Text Deck
} deriving Generic

instance D.FromDhall FlashBlastConfig

data MinimalReversedSpec = MinimalReversedSpec {
  from :: VF
, to   :: VF
} deriving (Eq, Show, Generic)

instance D.FromDhall MinimalReversedSpec
