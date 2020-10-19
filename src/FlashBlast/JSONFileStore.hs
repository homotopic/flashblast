{-# LANGUAGE UndecidableInstances #-}
module FlashBlast.JSONFileStore where

import Data.Aeson
import RIO
import qualified RIO.Map as Map
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.KVStore
import Path
import qualified UnliftIO.Path.Directory as U

newtype JSONFileStore = JSONFileStore (Path Rel File)
  deriving (Eq, Show, Generic)

newtype JSONParseException = JSONParseException String
  deriving (Show, Eq, Generic)

instance Exception JSONParseException where
  displayException (JSONParseException x) = x

eitherDecodeOrCreate :: (Monoid a, ToJSON a, FromJSON a, MonadIO m) => Path Rel File -> a -> m (Either String a)
eitherDecodeOrCreate f x = do
  whenM (fmap not . U.doesFileExist $ f) $ liftIO $ encodeFile (toFilePath f) x
  liftIO $ eitherDecodeFileStrict' (toFilePath f)

runKVStoreAsJSONFileStore :: (Members '[Embed IO, Input JSONFileStore, Error JSONParseException] r,
                              FromJSONKey k, ToJSONKey k, ToJSON k, FromJSON k, FromJSON v, ToJSON v, Ord k)
                          => Sem (KVStore k v ': r) a -> Sem r a
runKVStoreAsJSONFileStore = interpret \case
  LookupKV k -> do
    JSONFileStore f <- input @JSONFileStore
    z <- embed $ eitherDecodeOrCreate f mempty
    case z of
      Left x  -> throw @JSONParseException $ JSONParseException x
      Right x -> return $ Map.lookup k x
  UpdateKV k v -> do
    JSONFileStore f <- input @JSONFileStore
    z <- embed $ eitherDecodeOrCreate f mempty
    case z of
      Left x -> throw $ JSONParseException x
      Right (x :: Map k v) -> embed $ encodeFile (toFilePath f) (Map.alter (const v) k x)
