{- |
   Module     : FlashBlast.Domain
   License    : MIT
   Stability  : experimental

Top level domain specification for FlashBlast.
-}
{-# LANGUAGE TemplateHaskell #-}
module FlashBlast.Domain where

import RIO
import Polysemy
import Polysemy.Input
import Polysemy.Tagged
import Polysemy.Methodology
import Polysemy.Output

-- | A `DeckConfiguration` indicates how we create cards.
data DeckConfiguration

-- | A `CollectionsPackage` indicates.
data CollectionsPackage

-- | The Construction Methodology for flashblast.
data ConstructionMethodology

-- | `flashblast` is a program that takes a `DeckConfiguration` and outputs a `CollectionsPackage`.
flashblast :: Members '[ Tagged DeckConfiguration (Input a)
                       , Tagged ConstructionMethodology (Methodology a b)
                       , Tagged CollectionsPackage (Output b)] r
           => Sem r ()
flashblast = do
  x <- tag @DeckConfiguration input
  k <- tag @ConstructionMethodology $ process x
  tag @CollectionsPackage $ output k
