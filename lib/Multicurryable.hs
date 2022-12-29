{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Multicurryable where

import Data.SOP.NP
import Data.SOP.NS
import Data.Kind
import Data.Functor.Identity

type Multicurryable :: (Type -> Type -> Type) -> [Type] -> Type -> Type -> Constraint
class Multicurryable f items a curried | f items a -> curried , f curried -> items, f curried -> a where
    type UncurriedCollection f items :: Type
    multiuncurry :: curried -> f (UncurriedCollection f items) a
    multicurry :: f (UncurriedCollection f items) a -> curried

type family IsFunction f :: Bool where
    IsFunction (_ -> _) = 'True
    IsFunction _ = 'False

instance
    MulticurryableF (IsFunction curried) items a curried =>
        Multicurryable (->) items a curried where
    type UncurriedCollection (->) items = NP  Identity items
    multiuncurry = multiuncurryF @(IsFunction curried)
    multicurry = multicurryF @(IsFunction curried)

class MulticurryableF (b :: Bool) items a curried | items a -> curried , curried -> items, curried -> a where
    multiuncurryF :: curried -> NP Identity items -> a
    multicurryF :: (NP Identity items -> a) -> curried

someFunc :: IO ()
someFunc = putStrLn "someFunc"
