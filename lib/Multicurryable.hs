{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Multicurryable where

import Data.Functor.Identity
import Data.Kind
import Data.SOP.NP
import Data.SOP.NS

type Multicurryable :: (Type -> Type -> Type) -> [Type] -> Type -> Type -> Constraint
class Multicurryable f items a curried | f items a -> curried, f curried -> items, f curried -> a where
  type UncurriedCollection f :: [Type] -> Type
  multiuncurry :: curried -> f (UncurriedCollection f items) a
  multicurry :: f (UncurriedCollection f items) a -> curried

type family IsFunction f :: Bool where
  IsFunction (_ -> _) = 'True
  IsFunction _ = 'False

instance
  MulticurryableF (IsFunction curried) items a curried =>
  Multicurryable (->) items a curried
  where
  type UncurriedCollection (->) = NP Identity
  multiuncurry = multiuncurryF @(IsFunction curried)
  multicurry = multicurryF @(IsFunction curried)

class MulticurryableF (b :: Bool) items a curried 
  -- | b items a -> curried, b curried -> items, b curried -> a where
   | items a -> curried, curried b -> items, curried b -> a
    where
  multiuncurryF :: curried -> NP Identity items -> a
  multicurryF :: (NP Identity items -> a) -> curried

instance MulticurryableF 'False '[] a a where
  multiuncurryF = \a -> \Nil -> a
  multicurryF = \f -> f Nil

instance 
    MulticurryableF (IsFunction curried) rest tip curried =>
    MulticurryableF 'True (i ': rest) tip (i -> curried) where
  multiuncurryF f (Identity x :* rest) =
     multiuncurryF @(IsFunction curried) @rest @tip @curried (f x) rest
  multicurryF f i =
     multicurryF @(IsFunction curried) @rest @tip @curried $ \rest -> f (Identity i :* rest)


someFunc :: IO ()
someFunc = putStrLn "someFunc"


