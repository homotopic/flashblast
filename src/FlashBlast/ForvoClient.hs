{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module FlashBlast.ForvoClient where

import Data.Aeson
import Polysemy
import Polysemy.Error
import Polysemy.Input
import RIO hiding (fromException, try)
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
, sex :: Text
, country:: Text
, code :: Text
, langname :: Text
, pathmp3 ::Text
, pathogg :: Text
, rate :: Int
, num_votes :: Int
, num_positive_votes :: Int
} deriving (Eq, Show, Generic)

instance FromJSON ForvoPronunciationJson
instance ToJSON ForvoPronunciationJson

data ForvoAttributeCount = ForvoAttributeCount {
  total :: Int
} deriving (Eq, Show, Generic)

instance FromJSON ForvoAttributeCount

data ForvoLanguageListResponseBody = ForvoLanguageListResponseBody {
  attributes :: ForvoAttributeCount
, items :: [ForvoLanguageCode]
} deriving (Eq, Show, Generic)

instance FromJSON ForvoLanguageListResponseBody

data ForvoLanguageCode = ForvoLanguageCode {
  code :: Text
, language :: Text
} deriving (Eq, Show, Generic)

instance FromJSON ForvoLanguageCode

newtype MP3Url = MP3Url Text
  deriving (Eq, Show, Generic)

class HasMP3Url x where
  mp3Url :: Lens' x MP3Url

instance HasMP3Url ForvoPronunciationJson where
  mp3Url = lens (MP3Url . pathmp3) undefined

newtype OggUrl = OggUrl Text
  deriving (Eq, Show, Generic)

class HasOggUrl x where
  oggUrl :: Lens' x OggUrl

instance HasOggUrl ForvoPronunciationJson where
  oggUrl = lens (OggUrl . pathogg) undefined

data ForvoClient m a where
  StandardPronunciation :: Locale -> Text -> ForvoClient m ForvoStandardPronunciationResponseBody
  LanguageList          :: ForvoClient m ForvoLanguageListResponseBody
  MP3For                :: HasMP3Url x => x -> ForvoClient m ByteString
  OggFor                :: HasOggUrl x => x -> ForvoClient m ByteString

makeSem ''ForvoClient

newtype ForvoAPIKey = ForvoAPIKey Text
  deriving (Eq, Show, Generic)

data RemoteHttpRequest m a where
  RequestJSON :: FromJSON a => Text -> RemoteHttpRequest m a
  RequestBS   :: Text -> RemoteHttpRequest m ByteString

data ForvoLimitReachedException = ForvoLimitReachedException
  deriving (Eq, Show, Generic)

data ForvoLimitText = ForvoLimitText

instance FromJSON ForvoLimitText where
  parseJSON = withText "ForvoLimitText" $ \case
    "Limit/day reached." -> return ForvoLimitText
    _ -> fail "Unknown response."

data ForvoLimitResponse = ForvoLimitResponse [ForvoLimitText]
  deriving Generic

instance FromJSON ForvoLimitResponse

data ForvoAPIKeyIncorrectException = ForvoAPIKeyIncorrectException
  deriving (Eq, Show, Generic)

instance Exception ForvoAPIKeyIncorrectException where
  displayException = show

data ForvoIncorrectDomainText = ForvoIncorrectDomainText
  deriving (Eq, Show, Generic)

instance FromJSON ForvoIncorrectDomainText where
  parseJSON = withText "ForvoIncorrectDomainText" $ \case
    "Calling from incorrect domain." -> return ForvoIncorrectDomainText
    _ -> fail "Unknown response."

data ForvoIncorrectDomainResponse = ForvoIncorrectDomainResponse [ForvoIncorrectDomainText]
  deriving Generic

instance FromJSON ForvoIncorrectDomainResponse

data ForvoResponseNotUnderstood = ForvoResponseNotUnderstood ByteString
  deriving (Show, Eq, Generic)

instance Exception ForvoResponseNotUnderstood where
  displayException (ForvoResponseNotUnderstood x) = show x

makeSem ''RemoteHttpRequest

data BadRequestException where
  BadRequestException :: Exception e => e -> BadRequestException

deriving instance Show BadRequestException

instance Exception BadRequestException where
  displayException = show

interpretRemoteHttpRequest :: Members '[Embed IO, Error JSONException, Error BadRequestException] r => Sem (RemoteHttpRequest ': r) a -> Sem r a
interpretRemoteHttpRequest = interpret \case
  RequestJSON x -> do
    let k = parseRequestThrow $ T.unpack x
    case k of
      Left e -> throw @BadRequestException $ BadRequestException e
      Right x' -> do
        j <- fromException @JSONException $ httpJSON x'
        return $ getResponseBody j
  RequestBS x -> do
    let k = parseRequest $ T.unpack x
    case k of
      Left e -> throw @BadRequestException $ BadRequestException e
      Right x' -> do
        j <- httpBS x'
        return $ getResponseBody j

throwIfLimitReached :: Member (Error ForvoLimitReachedException) r => ByteString -> Sem r ByteString
throwIfLimitReached z = let (z' :: Either String ForvoLimitResponse) = eitherDecodeStrict z
                        in case z' of
                          Left _  -> return z
                          Right _ -> throw ForvoLimitReachedException

throwIfIncorrectDomain :: Member (Error ForvoAPIKeyIncorrectException) r => ByteString -> Sem r ByteString
throwIfIncorrectDomain z = let (z' :: Either String ForvoIncorrectDomainResponse) = eitherDecodeStrict z
                        in case z' of
                          Left _  -> return z
                          Right _ -> throw ForvoAPIKeyIncorrectException


interpretForvoClient :: Members '[ RemoteHttpRequest
                                 , Input ForvoAPIKey
                                 , Error ForvoLimitReachedException
                                 , Error ForvoAPIKeyIncorrectException
                                 , Error ForvoResponseNotUnderstood] r => Sem (ForvoClient ': r) a -> Sem r a
interpretForvoClient = interpret \case
  StandardPronunciation (Locale l) t -> do
    ForvoAPIKey f <- input @ForvoAPIKey
    let k = "https://apifree.forvo.com/key/" <> f <> "/format/json/action/standard-pronunciation/word/" <> t <> "/language/" <> l
    z <- requestBS k
    _ <- throwIfLimitReached z
    _ <- throwIfIncorrectDomain z
    let (y' :: Either String ForvoStandardPronunciationResponseBody) = eitherDecodeStrict z
    case y' of
      Left _ -> throw @ForvoResponseNotUnderstood (ForvoResponseNotUnderstood z)
      Right x -> return x
  LanguageList -> do
    ForvoAPIKey f <- input @ForvoAPIKey
    let k = "https://apifree.forvo.com/key/" <> f <> "/format/json/action/language-list/order/name"
    z <- requestBS k
    _ <- throwIfLimitReached z
    _ <- throwIfIncorrectDomain z
    let (y' :: Either String ForvoLanguageListResponseBody) = eitherDecodeStrict z
    case y' of
      Left _ -> throw @ForvoResponseNotUnderstood (ForvoResponseNotUnderstood z)
      Right x -> return x
  MP3For x -> let (MP3Url x') = view mp3Url x
              in requestBS x' >>= throwIfLimitReached
  OggFor x -> let (OggUrl x') = view oggUrl x
              in requestBS x' >>= throwIfLimitReached
