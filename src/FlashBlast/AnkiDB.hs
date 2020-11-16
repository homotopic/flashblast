{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module FlashBlast.AnkiDB where

import           Path
import           Polysemy
import           Polysemy.Reader
import           RIO                     hiding (Reader, asks)
import           UnliftIO.Path.Directory
