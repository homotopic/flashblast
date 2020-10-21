{-# LANGUAGE TemplateHaskell #-}
module FlashBlast.FS where

import Polysemy
import qualified UnliftIO.Path.Directory as U
import qualified System.IO.Temp as U
import Path
import qualified RIO as RIO
import RIO (Text, Bool(..), IO, (>>=), ($), return, (.), fmap)
import qualified RIO.ByteString as BS
import Colog.Polysemy
import Colog.Polysemy.Formatting
import Formatting
import Path.Utils
import GHC.Stack

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

data FileExists where
  FileExists    :: Path b File -> FileExists
  FileNotExists :: Path b File -> FileExists

data DirExists where
  DirExists    :: Path b Dir -> DirExists
  DirNotExists :: Path b Dir -> DirExists

logFileExists :: Members '[FSExist, Log FileExists] r => Sem r a -> Sem r a
logFileExists = intercept \case
  DoesFileExist x -> do
    z <- doesFileExist x
    case z of
      True  -> log $ FileExists x
      False -> log $ FileNotExists x
    return z
  DoesDirExist x -> doesDirExist x

logDirExists :: Members '[FSExist, Log DirExists] r => Sem r a -> Sem r a
logDirExists = intercept \case
  DoesDirExist x  -> do
    z <- doesDirExist x
    case z of
      True  -> log $ DirExists x
      False -> log $ DirNotExists x
    return z
  DoesFileExist x -> doesFileExist x

mapLog :: forall k x r a. Members '[Log x] r => (k -> x) -> Sem (Log k ': r) a -> Sem r a
mapLog f = interpret $ \(Log x) -> log (f x)

logInfo' :: HasCallStack => Text -> Msg Severity
logInfo' = Msg Info callStack

path :: Format r (Path b t -> r)
path = fmap (. toFilePathText) stext

fileExistsLogText :: FileExists -> Text
fileExistsLogText = \case
  FileExists f    -> sformat ("File " % path % " found in filesystem.") f
  FileNotExists f -> sformat ("File " % path % " not found in filesystem.") f
