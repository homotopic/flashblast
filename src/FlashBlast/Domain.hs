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

data Methodology b c m a where
  Process :: b -> Methodology b c m c

makeSem ''Methodology

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

runMethodology :: (b -> Sem r c) -> Sem (Methodology b c ': r) a -> Sem r a
runMethodology f = interpret \case
  Process b -> f b

cmapMethodology :: (b -> d) -> Sem (Methodology b c ': r) a -> Sem (Methodology d c ': r) a
cmapMethodology f = reinterpret \case
  Process b -> process $ f b

divideMethodology :: forall b d e x y c r a. (b -> (d, e)) -> ((x, y) -> c) -> Sem (Methodology b c ': r) a -> Sem (Methodology d x ': Methodology e y ': r) a
divideMethodology f g = reinterpret2 \case
  Process b -> do
    let (d', e') = f b
    x' <- process @d d'
    y' <- raise $ process @e @y e'
    return $ g (x', y')

decideMethodology :: forall b d e x y c r a.
                     (b -> Either d e)
                  -> Sem (Methodology b c ': r) a
                  -> Sem (Methodology d c ': Methodology e c ': r) a
decideMethodology f = reinterpret2 \case
  Process b -> case f b of
                Left d' -> process @d @c d'
                Right e' -> raise $ process @e @c e'


endMethodology :: Monoid c => Sem (Methodology (HList '[]) c ': r) a -> Sem r a
endMethodology = interpret \case
  Process _ -> return $ mempty

runSubMethodology :: forall c x xs r a. (Member (Methodology (HList xs) c) r, Monoid c)
                  => (x -> Sem r c)
                  -> Sem (Methodology (HList (x ': xs)) c ': r) a
                  -> Sem r a
runSubMethodology f = interpret \case
  Process (x ::: xs) -> liftA2 (<>) (f x) (process @(HList xs) @c xs)
