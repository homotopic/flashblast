{- |
   Module     : FlashBlast.Domain
   License    : MIT
   Stability  : experimental

Top level domain specification for FlashBlast.
-}
module FlashBlast.Domain where

import Data.Kind
import Fcf
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

-- | Cards can be of the following types.
data CardType = Minimal | Basic | Excerpt | Pronunciation

-- | They have a way to configure them.
type family ConfigFor (a :: CardType) :: Type

-- | And a concrete product.
type family ResultFor (a :: CardType) :: Type

-- | Fcf mapping for CardType to its Config.
data ConfigFor'  :: CardType -> Exp Type
type instance Eval (ConfigFor' x) = ConfigFor x

-- | Fcf mapping for CardType to its product.
data ResultFor'  :: CardType -> Exp Type
type instance Eval (ResultFor' x) = ResultFor x
