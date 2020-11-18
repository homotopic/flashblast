{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module FlashBlast.ForvoClient where

import Data.Aeson
import Polysemy
import Polysemy.Error
import Polysemy.Input
import RIO hiding (fromException, try)
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.ByteString as BS
import Polysemy.Http as Http

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

newtype ForvoAttributeCount = ForvoAttributeCount {
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

data ForvoLimitReachedException = ForvoLimitReachedException
  deriving (Eq, Show, Generic)

data ForvoLimitText = ForvoLimitText

instance FromJSON ForvoLimitText where
  parseJSON = withText "ForvoLimitText" $ \case
    "Limit/day reached." -> return ForvoLimitText
    _ -> fail "Unknown response."

instance Exception ForvoLimitReachedException where
  displayException = show

newtype ForvoLimitResponse = ForvoLimitResponse [ForvoLimitText]
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

newtype ForvoIncorrectDomainResponse = ForvoIncorrectDomainResponse [ForvoIncorrectDomainText]
  deriving Generic

instance FromJSON ForvoIncorrectDomainResponse

newtype ForvoResponseNotUnderstood = ForvoResponseNotUnderstood ByteString
  deriving (Show, Eq, Generic)

instance Exception ForvoResponseNotUnderstood where
  displayException (ForvoResponseNotUnderstood x) = show x

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

validate :: forall r. Members [Error ForvoLimitReachedException
                             , Error ForvoAPIKeyIncorrectException
                             , Error HttpError] r
          => Either HttpError (Response LBS.ByteString) -> Sem r ByteString
validate z = do
        z' <- either throw (return . LBS.toStrict . body) z
        _ <- throwIfLimitReached z'
        _ <- throwIfIncorrectDomain z'
        return z'

analyse :: forall a r. (FromJSON a,
           Members '[Error ForvoResponseNotUnderstood] r)
        => BS.ByteString -> Sem r a
analyse z = do
        either
          (const $ throw @ForvoResponseNotUnderstood . ForvoResponseNotUnderstood $ z)
          return
          (eitherDecodeStrict z :: Either String a)

runForvoClient :: Members '[ Input ForvoAPIKey
                           , Error ForvoLimitReachedException
                           , Error ForvoAPIKeyIncorrectException
                           , Error HttpError
                           , Error ForvoResponseNotUnderstood
                           , Http (IO ByteString)] r => Sem (ForvoClient ': r) a -> Sem r a
runForvoClient = interpret \case
  StandardPronunciation (Locale l) t -> do
    ForvoAPIKey f <- input @ForvoAPIKey
    z <- Http.request (Http.get "apifree.forvo.com" $ Path $ "key/" <> f <> "/format/json/action/standard-pronunciation/word/" <> t <> "/language/" <> l)
    validate z >>= analyse
  LanguageList -> do
    ForvoAPIKey f <- input @ForvoAPIKey
    z <- Http.request (Http.get "apifree.forvo.com" $ Path $ "key/" <> f <> "/format/json/action/language-list/order/name")
    validate z >>= analyse
  MP3For x -> let (MP3Url x') = view mp3Url x
              in do
                let Right k = getUrl x'
                request k >>= validate
  OggFor x -> let (OggUrl x') = view oggUrl x
              in do
                let Right k = getUrl x'
                request k >>= validate
