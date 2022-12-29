{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Multicurryable (Multicurryable (..)) where

import Data.Functor.Identity
import Data.Kind
import Data.SOP.NP
import Data.SOP.NS

type Multicurryable :: (Type -> Type -> Type) -> [Type] -> Type -> Type -> Constraint
class
  Multicurryable f items a curried
    | f items a -> curried,
      f curried -> items,
      f curried -> a
  where
  type UncurriedCollection f :: [Type] -> Type
  multiuncurry :: curried -> f (UncurriedCollection f items) a
  multicurry :: f (UncurriedCollection f items) a -> curried

-- Instance for (->)

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

class
  MulticurryableF (b :: Bool) items a curried
    | items a -> curried,
      curried b -> items,
      curried b -> a
  where
  multiuncurryF :: curried -> NP Identity items -> a
  multicurryF :: (NP Identity items -> a) -> curried

instance MulticurryableF 'False '[] a a where
  multiuncurryF a Nil = a
  multicurryF f = f Nil

instance
  MulticurryableF (IsFunction curried) rest tip curried =>
  MulticurryableF 'True (i ': rest) tip (i -> curried)
  where
  multiuncurryF f (Identity x :* rest) =
    multiuncurryF @(IsFunction curried) @rest @tip @curried (f x) rest
  multicurryF f i =
    multicurryF @(IsFunction curried) @rest @tip @curried $ \rest -> f (Identity i :* rest)

-- Instance for Either

type family IsEither f :: Bool where
  IsEither (Either _ _) = 'True
  IsEither _ = 'False

instance
  MulticurryableE (IsEither curried) items a curried =>
  Multicurryable Either items a curried
  where
  type UncurriedCollection Either = NS Identity
  multiuncurry = multiuncurryE @(IsEither curried)
  multicurry = multicurryE @(IsEither curried)

class
  MulticurryableE (b :: Bool) items a curried
    | items a -> curried,
      curried b -> items,
      curried b -> a
  where
  multiuncurryE :: curried -> Either (NS Identity items) a
  multicurryE :: Either (NS Identity items) a -> curried

instance MulticurryableE 'False '[] a a where
  multiuncurryE = Right
  multicurryE = \case
    Left impossible -> case impossible of {}
    Right a -> a

instance
  MulticurryableE (IsEither curried) rest tip curried =>
  MulticurryableE 'True (i ': rest) tip (Either i curried)
  where
  multiuncurryE = \case
    Left x -> Left (Z (Identity x))
    Right rest ->
      case multiuncurryE @(IsEither curried) @rest @tip @curried rest of
        Left ns -> Left (S ns)
        Right x -> Right x
  multicurryE = \case
    Left rest -> case rest of
      Z (Identity x) -> Left x
      S rest' -> Right $ multicurryE @(IsEither curried) @_ @tip @curried (Left rest')
    Right tip -> Right $ multicurryE @(IsEither curried) @_ @tip @curried (Right tip)