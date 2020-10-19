{-# LANGUAGE TemplateHaskell #-}
module FlashBlast.FBFileSystem where

import Polysemy
import qualified UnliftIO.Path.Directory as U
import qualified System.IO.Temp as U
import Path
import RIO
import qualified RIO.ByteString as BS

data FBFileSystem m a where
  CreateTempDirectory :: FBFileSystem m (Path Abs Dir)
  CreateDirectory :: Path b Dir -> FBFileSystem m ()
  RemoveDirectory :: Path b Dir -> FBFileSystem m ()
  DoesFileExist :: Path b File -> FBFileSystem m Bool
  CopyFile :: Path b File -> Path b' File -> FBFileSystem m ()
  WriteFileBS :: Path b File -> BS.ByteString -> FBFileSystem m ()

makeSem ''FBFileSystem

interpretFBFileSystem :: Member (Embed IO) effs => Sem (FBFileSystem ': effs) a -> Sem effs a
interpretFBFileSystem = interpret \case
  CreateTempDirectory -> do
    x <- embed U.getCanonicalTemporaryDirectory
    embed $ U.createTempDirectory x "" >>= parseAbsDir
  CreateDirectory x -> U.createDirectoryIfMissing True x
  RemoveDirectory x   -> U.removeDirectoryRecursive x
  DoesFileExist x     -> U.doesFileExist x
  CopyFile x y -> U.copyFile x y
  WriteFileBS x y -> BS.writeFile (toFilePath x) y
