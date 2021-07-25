{-# LANGUAGE TemplateHaskell #-}

-- |
--   Module     : FlashBlast.VF
--   License    : MIT
--   Stability  : experimental
--
-- Variable mediafield for dhall.
module FlashBlast.VF
  ( VF (..),
    VFC (..),
    VFRF,
    VFCRF,
  )
where

import Dhall
import Media.VF
import Optics
import Path
import Path.Dhall ()
import RIO

type VFRF = VF (Path Rel File)

type VFCRF = VFC (Path Rel File)
