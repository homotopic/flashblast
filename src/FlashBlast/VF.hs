{- |
   Module     : FlashBlast.VF
   License    : MIT
   Stability  : experimental

Variable mediafield for dhall.
-}
{-# LANGUAGE TemplateHaskell #-}
module FlashBlast.VF (
  VF(..)
, VFC(..)
) where

import Optics
import Dhall
import Path
import Path.Dhall()
import RIO

data VF where
  Blank      :: VF
  RawText    :: Text -> VF
  Image      :: Path Rel File -> VF
  Audio      :: Path Rel File -> VF
  Video      :: Path Rel File -> VF

makePrisms ''VF

deriving instance Generic VF
deriving instance Eq      VF
deriving instance Show    VF

instance FromDhall VF
instance ToDhall VF

data VFC where
  Single :: VF -> VFC
  Multi  :: [VF] -> VFC

makePrisms ''VFC

deriving instance Generic VFC
deriving instance Eq      VFC
deriving instance Show    VFC

instance FromDhall VFC
instance ToDhall VFC
