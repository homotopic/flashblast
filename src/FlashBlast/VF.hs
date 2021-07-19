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
import Optics
import Path
import Path.Dhall ()
import RIO

data VF u where
  Blank :: VF u
  RawText :: Text -> VF u
  Image :: u -> VF u
  Audio :: u -> VF u
  Video :: u -> VF u

makePrisms ''VF

deriving instance Generic (VF u)

deriving instance Functor VF

deriving instance Eq u => Eq (VF u)

deriving instance Show u => Show (VF u)

instance (Generic u, FromDhall u) => FromDhall (VF u)

instance (Generic u, ToDhall u) => ToDhall (VF u)

data VFC u where
  Single :: VF u -> VFC u
  Multi :: [VF u] -> VFC u

makePrisms ''VFC

deriving instance Generic (VFC u)

deriving instance Eq u => Eq (VFC u)

deriving instance Show u => Show (VFC u)

instance (Generic u, FromDhall u) => FromDhall (VFC u)

instance (Generic u, ToDhall u) => ToDhall (VFC u)

type VFRF = VF (Path Rel File)

type VFCRF = VFC (Path Rel File)
