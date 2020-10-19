{-# LANGUAGE TemplateHaskell #-}
module FlashBlast.ForvoClient where

import Data.Aeson
import Polysemy
import Polysemy.Error
import Polysemy.Input
import RIO hiding (fromException)
import qualified RIO.Text as T
import Network.HTTP.Simple

newtype Locale = Locale Text
  deriving (Eq, Show, Generic, Ord)

instance ToJSON Locale
instance FromJSON Locale

newtype ForvoStandardPronunciationResponseBody = ForvoStandardPronunciationResponseBody {
  items :: [ForvoPronunciationJson]
} deriving (Eq, Show, Generic)

instance FromJSON ForvoStandardPronunciationResponseBody
instance ToJSON ForvoStandardPronunciationResponseBody

data ForvoPronunciationJson = ForvoPronunciationJson {
  id :: Int
, word :: Text
, original :: Text
, hits :: Int
, username:: Text
, sex:: Text
, country:: Text
, code :: Text
, langname :: Text
, pathmp3 ::Text
, pathogg :: Text
, rate:: Int
, num_votes :: Int
, num_positive_votes:: Int
} deriving (Eq, Show, Generic)

instance FromJSON ForvoPronunciationJson
instance ToJSON ForvoPronunciationJson

newtype MP3Url = MP3Url Text
  deriving (Eq, Show, Generic)

class HasMP3Url x where
  mp3Url :: Lens' x MP3Url

instance HasMP3Url ForvoPronunciationJson where
  mp3Url = lens (MP3Url . pathmp3) undefined

data ForvoClient m a where
  StandardPronunciation :: Locale -> Text -> ForvoClient m ForvoStandardPronunciationResponseBody
  MP3For :: HasMP3Url x => x -> ForvoClient m ByteString

makeSem ''ForvoClient

newtype ForvoAPIKey = ForvoAPIKey Text
  deriving (Eq, Show, Generic)

data RemoteHttpRequest m a where
  RequestJSON :: FromJSON a => Text -> RemoteHttpRequest m a
  RequestBS   :: Text -> RemoteHttpRequest m ByteString

makeSem ''RemoteHttpRequest

interpretRemoteHttpRequest :: Members '[Embed IO, Error JSONException, Error SomeException] r => Sem (RemoteHttpRequest ': r) a -> Sem r a
interpretRemoteHttpRequest = interpret \case
  RequestJSON x -> do
    let k = parseRequest $ T.unpack x
    case k of
      Left e -> throw @SomeException e
      Right x -> do
        j <- fromException @JSONException $ httpJSON x
        return $ getResponseBody j
  RequestBS x -> do
    let k = parseRequest $ T.unpack x
    case k of
      Left e -> throw @SomeException e
      Right x -> do
        j <- fromException @JSONException $ httpBS x
        return $ getResponseBody j

interpretForvoClient :: Members '[RemoteHttpRequest, Input ForvoAPIKey] r => Sem (ForvoClient ': r) a -> Sem r a
interpretForvoClient = interpret \case
  StandardPronunciation (Locale l) t -> do
    ForvoAPIKey f <- input @ForvoAPIKey
    let k = "https://apifree.forvo.com/key/" <> f <> "/format/json/action/standard-pronunciation/word/" <> t <> "/language/" <> l
    requestJSON k
  MP3For x -> let (MP3Url x') = view mp3Url x
              in requestBS x'
