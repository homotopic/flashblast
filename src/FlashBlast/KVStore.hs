module FlashBlast.KVStore where

import Path
import Polysemy.Input
import Polysemy.Tagged
import Optics
import RIO hiding (view)
import Polysemy
import FlashBlast.FS
import Polysemy.KVStore


type FSKVStore b = KVStore (Path b File) ByteString

runFSKVStoreRelIn :: Members '[FSExist, FSRead, FSWrite, FSDir] r => Path b Dir -> Sem (FSKVStore Rel ': r) a -> Sem r a
runFSKVStoreRelIn d = interpret \case
  LookupKV k   -> do
    createDirectory d
    z <- doesFileExist (d </> k)
    case z of
      True  -> fmap Just . readFileBS $ d </> k
      False -> return Nothing
  UpdateKV k v -> do
    createDirectory d
    case v of
      Nothing -> pure ()
      Just x  -> writeFileBS (d </> k) x

hotKVStore :: forall t t' k v r. Members '[Input k, Tagged t (KVStore k v), Tagged t' (KVStore k v)] r => Sem r ()
hotKVStore = do
  a <- input @k
  z <- tag @t @(KVStore k v) $ existsKV @k @v a
  if z
    then return ()
  else
    do
      x <- tag @t' @(KVStore k v) $ lookupKV @k @v a
      case x of
        Nothing -> return ()
        Just x' -> tag @t @(KVStore k v) $ writeKV @k @v a x'

runKVStoreAsKVStore :: forall k v k' v' r a. Getter k k' -> Iso' v v' -> Sem (KVStore k v ': r) a -> Sem (KVStore k' v' ': r) a
runKVStoreAsKVStore f g = reinterpret \case
  LookupKV k   -> fmap (review g) <$> lookupKV @k' @v' (view f k)
  UpdateKV k x -> updateKV @k' @v' (view f k) (fmap (view g) x)
