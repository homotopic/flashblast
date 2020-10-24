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
import Polysemy.Output
import Polysemy.Several

-- | A `DeckConfiguration` indicates how we create cards.
data DeckConfiguration

-- | A `CollectionsPackage` indicates
data CollectionsPackage

-- | `flashblast` is a program that takes a `DeckConfiguration` and outputs a `CollectionsPackage`.
flashblast :: Members '[ Tagged DeckConfiguration (Input a)
                       , Tagged CollectionsPackage (Output b)] r
           => (a -> Sem r b)
           -> Sem r ()
flashblast f = do
  x <- tag @DeckConfiguration input
  k <- f x
  tag @CollectionsPackage $ output k

data Methodology c b m a where
  Process :: b -> Methodology c b m c

makeSem ''Methodology

runMethodology :: (b -> Sem r c) -> Sem (Methodology c b ': r) a -> Sem r a
runMethodology f = interpret \case
  Process b -> f b

contramapMethodology :: (b -> a) -> Sem (Methodology c b ': r) a -> Sem (Methodology c a ': r) a
contramapMethodology f = reinterpret \case
  Process b -> process $ f b

endMethodology :: Monoid c => Sem (Methodology c (HList '[]) ': r) a -> Sem r a
endMethodology = interpret \case
  Process _ -> return $ mempty

runSubMethodology :: forall c x xs r a. (Member (Methodology c (HList xs)) r, Monoid c) => (x -> Sem r c) -> Sem (Methodology c (HList (x ': xs)) ': r) a -> Sem r a
runSubMethodology f = interpret \case
  Process (x ::: xs) -> liftA2 (<>) (f x) (process @c @(HList xs) xs)
