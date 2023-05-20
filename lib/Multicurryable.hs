{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | While writing function decorators, we often need to store the arguments
-- of the function in a n-ary product. @'multiuncurry' \@(<-)@ is useful for that.   
--
-- >>> :{
-- type Fun0 = Int
-- type UFun0 = NP I '[] -> Int
-- type Fun1 = Bool -> Int
-- type UFun1 = NP I '[Bool] -> Int
-- type Fun2 = Char -> Bool -> Int
-- type UFun2 = NP I '[Char, Bool] -> Int
-- ufun0 :: UFun0 = multiuncurry @(->) @_ @_ @Fun0 $ 5
-- ufun1 :: UFun1 = multiuncurry @(->) @_ @_ @Fun1 $ \_ -> 5
-- ufun2 :: UFun2 = multiuncurry @(->) @_ @_ @Fun2 $ \_ _ -> 5
-- fun0 :: Fun0 = multicurry @(->) @_ @_ ufun0 
-- fun1 :: Fun1 = multicurry @(->) @_ @_ ufun1 
-- fun2 :: Fun2 = multicurry @(->) @_ @_ ufun2
-- :}
--
--
-- Less often, when processing the result of functions, we have a nested chain
-- of 'Either's like @Either Err1 (Either Err2 (Either Err3 Success))@, and want to put all the errors in a top-level 'Left' branch,
-- and the lone @Success@ value in a top-level 'Right' branch. @'multiuncurry' \@Either@ is useful for that.   
-- 
-- >>> :{
-- type Eith0 = Int
-- type Eith1 = Either Bool Int
-- type Eith2 = Either Char (Either Bool Int)
-- type UEith0 = Either (NS I '[]) Int
-- type UEith1 = Either (NS I '[Bool]) Int
-- type UEith2 = Either (NS I '[Char, Bool]) Int
-- ueith0 :: UEith0 = multiuncurry @Either @_ @_ @Eith0 5
-- ueith1 :: UEith1 = multiuncurry @Either @_ @_ @Eith1 $ Right 5
-- ueith2 :: UEith2 = multiuncurry @Either @_ @_ @Eith2 $ Right (Right 5)
-- eith0 :: Eith0 = multicurry @Either @_ @_ ueith0
-- eith1 :: Eith1 = multicurry @Either @_ @_ ueith1
-- eith2 :: Eith2 = multicurry @Either @_ @_ ueith2
-- :}
--
--
-- The 'Multicurryable' class will get terribly confused if it can't determine
-- the rightmost type, because it can't be sure it's not another @(->)@, or
-- another @Either@. So use it only with concrete rightmost types, not
-- polymorphic ones.
-- 
module Multicurryable (
    -- * Multi-argument currying/uncurrying.
    Multicurryable (..),
    -- * Helpers for '(->)'
    MulticurryableF,
    IsFunction,
    -- * Helpers for 'Either'
    MulticurryableE,
    IsEither,
    -- * sop-core re-exports
    NP (..),
    NS (..),
    I (..),
  ) where

import Data.Kind
import Data.SOP

type Multicurryable :: (Type -> Type -> Type) -> [Type] -> Type -> Type -> Constraint
class
  Multicurryable (f :: Type -> Type -> Type) (items :: [Type]) a curried
    | f items a -> curried,
      f curried -> items a
  where
  type UncurriedArgs f :: [Type] -> Type
  multiuncurry :: curried -> f (UncurriedArgs f items) a
  multicurry :: f (UncurriedArgs f items) a -> curried

-- Instance for (->)

type IsFunction :: Type -> Where
type family IsFunction f :: Where where
  IsFunction (_ -> _) = 'NotYetThere 
  IsFunction _ = 'AtTheTip

  
-- | The instance for functions provides conventional currying/uncurrying, only
-- that it works for multiple arguments, and the uncurried arguments are stored
-- in a 'NP' product instead of a tuple.
instance
  MulticurryableF items a curried (IsFunction curried)
  =>
  Multicurryable (->) items a curried
  where
  type UncurriedArgs (->) = NP I
  multiuncurry = multiuncurryF @_ @_ @_ @(IsFunction curried)
  multicurry = multicurryF @_ @_ @_ @(IsFunction curried)

type MulticurryableF :: [Type] -> Type -> Type -> Where -> Constraint
class
  MulticurryableF items a curried (decomp :: Where)
    | items a -> curried decomp,
      curried decomp -> items a 
  where
  multiuncurryF :: curried -> NP I items -> a
  multicurryF :: (NP I items -> a) -> curried

instance MulticurryableF '[] a a 'AtTheTip where
  multiuncurryF a Nil = a
  multicurryF f = f Nil

instance
  MulticurryableF rest tip curried (IsFunction curried)
  =>
  MulticurryableF (i ': rest) tip (i -> curried) 'NotYetThere
  where
  multiuncurryF f (I x :* rest) =
    multiuncurryF @rest @tip @curried @(IsFunction curried) (f x) rest
  multicurryF f i =
    multicurryF @rest @tip @curried @(IsFunction curried) $ \rest -> f (I i :* rest)

-- Instance for Either

type IsEither :: Type -> Where
type family IsEither f :: Where where
  IsEither (Either _ _) = 'NotYetThere
  IsEither _ = 'AtTheTip


-- | The instance for 'Either' takes a sequence nested 'Either's, separates the
-- errors from the success value at the right tip, and stores any occurring
-- errors in a 'NS' sum.
instance
  MulticurryableE items a curried (IsEither curried) =>
  Multicurryable Either items a curried
  where
  type UncurriedArgs Either = NS I
  multiuncurry = multiuncurryE @_ @_ @_ @(IsEither curried)
  multicurry = multicurryE @_ @_ @_ @(IsEither curried)


type MulticurryableE :: [Type] -> Type -> Type -> Where -> Constraint
class
  MulticurryableE items a curried (decomp :: Where)
    | items a -> curried decomp,
      curried decomp -> items a
  where
  multiuncurryE :: curried -> Either (NS I items) a
  multicurryE :: Either (NS I items) a -> curried

instance MulticurryableE '[] a a 'AtTheTip where
  multiuncurryE = Right
  multicurryE = \case
    Left impossible -> case impossible of {}
    Right a -> a

instance
  MulticurryableE  rest tip curried (IsEither curried) =>
  MulticurryableE (i ': rest) tip (Either i curried) 'NotYetThere
  where
  multiuncurryE = \case
    Left x -> Left (Z (I x))
    Right rest ->
      case multiuncurryE @rest @tip @curried @(IsEither curried) rest of
        Left ns -> Left (S ns)
        Right x -> Right x
  multicurryE = \case
    Left rest -> case rest of
      Z (I x) -> Left x
      S rest' -> Right $ multicurryE @_ @tip @curried @(IsEither curried) (Left rest')
    Right tip -> Right $ multicurryE @_ @tip @curried @(IsEither curried) (Right tip)

-- 
data Where =
        NotYetThere
      | AtTheTip

-- $setup
-- >>> :set -XAllowAmbiguousTypes
-- >>> :set -XDataKinds 
-- >>> :set -XFunctionalDependencies 
-- >>> :set -XLambdaCase 
-- >>> :set -XTypeFamilies
-- >>> :set -XTypeOperators
-- >>> :set -XUndecidableInstances 
-- >>> import Multicurryable
-- >>> import Data.SOP