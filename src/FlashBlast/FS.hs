{-# LANGUAGE TemplateHaskell #-}
module FlashBlast.FS where

import Polysemy
import qualified UnliftIO.Path.Directory as U
import qualified System.IO.Temp as U
import Path
import qualified RIO as RIO
import RIO (Text, Bool(..), IO, (>>=), ($))
import qualified RIO.ByteString as BS

data FSExist m a where
  DoesFileExist :: Path b File -> FSExist m Bool
  DoesDirExist  :: Path b Dir  -> FSExist m Bool

makeSem ''FSExist

data FSRead m a where
  ReadFileBS   :: Path b File -> FSRead m BS.ByteString
  ReadFileUtf8 :: Path b File -> FSRead m Text

makeSem ''FSRead

data FSWrite m a where
  WriteFileBS   :: Path b File -> BS.ByteString -> FSWrite m ()
  WriteFileUtf8 :: Path b File -> Text -> FSWrite m ()

makeSem ''FSWrite

data FSCopy m a where
  CopyFile :: Path b File -> Path b' File -> FSCopy m ()

makeSem ''FSCopy

data FSTemp m a where
  CreateTempDirectory :: FSTemp m (Path Abs Dir)

makeSem ''FSTemp

data FSDir m a where
  CreateDirectory :: Path b Dir -> FSDir m ()
  RemoveDirectory :: Path b Dir -> FSDir m ()

makeSem ''FSDir

runFSExistIO :: Member (Embed IO) r => Sem (FSExist ': r) a -> Sem r a
runFSExistIO = interpret \case
  DoesFileExist x -> U.doesFileExist x
  DoesDirExist x  -> U.doesDirectoryExist  x

runFSReadIO :: Member (Embed IO) r => Sem (FSRead ': r) a -> Sem r a
runFSReadIO = interpret \case
  ReadFileBS x   -> BS.readFile (toFilePath x)
  ReadFileUtf8 x -> RIO.readFileUtf8 (toFilePath x)

runFSWriteIO :: Member (Embed IO) r => Sem (FSWrite ': r) a -> Sem r a
runFSWriteIO = interpret \case
  WriteFileBS x y -> BS.writeFile (toFilePath x) y
  WriteFileUtf8 x y -> RIO.writeFileUtf8 (toFilePath x) y

runFSCopyIO :: Member (Embed IO) r => Sem (FSCopy ': r) a -> Sem r a
runFSCopyIO = interpret \case
  CopyFile x y -> U.copyFile x y

runFSDirIO :: Member (Embed IO) r => Sem (FSDir ': r) a -> Sem r a
runFSDirIO = interpret \case
  CreateDirectory x -> U.createDirectoryIfMissing True x
  RemoveDirectory x -> U.removeDirectoryRecursive x

runFSTempIO :: Member (Embed IO) r => Sem (FSTemp ': r) a -> Sem r a
runFSTempIO = interpret \case
  CreateTempDirectory -> do
    x <- embed U.getCanonicalTemporaryDirectory
    embed $ U.createTempDirectory x "" >>= parseAbsDir
