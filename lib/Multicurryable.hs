{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | While writing function decorators, we often need to store the arguments
-- of the function in a n-ary product. @'multiuncurry' \@(<-)@ is useful for that.   
--
-- >>> :{
-- type Fun0 = Int
-- type Fun1 = Bool -> Int
-- type Fun2 = Char -> Bool -> Int
-- ufun0 = multiuncurry @(->) @_ @_ @Fun0 $ 5
-- ufun1 = multiuncurry @(->) @_ @_ @Fun1 $ \_ -> 5
-- ufun2 = multiuncurry @(->) @_ @_ @Fun2 $ \_ _ -> 5
-- :}
--
--
-- Less often, when processing the result of functions, we have a nested chain
-- of 'Either's like @Either Err1 (Either Err2 (Either Err3 Success))@, and want to put all the errors in a top-level 'Left' branch,
-- and the lone @Success@ value in a top-level 'Right' branch. @'multiuncurry' \@Either@ is useful for that.   
-- 
-- The 'Multicurryable' class will get terribly confused if it can't determine
-- the rightmost type, because it can't be sure it's not another @(->)@, or
-- another @Either@. So use it only with concrete rightmost types, not
-- polymorphic ones.
-- 
module Multicurryable (
    -- * Multi-argument currying/uncurrying.
    Multicurryable (..),
    -- * sop-core re-exports
    NP (..),
    NS (..),
    I (..),
  ) where

import Data.Kind
import Data.SOP
import Data.SOP.NP
import Data.SOP.NS

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

type family IsFunction f :: Bool where
  IsFunction (_ -> _) = 'True
  IsFunction _ = 'False

-- | The instance for functions provides conventional currying/uncurrying, only
-- that it works for multiple arguments, and the uncurried arguments are stored
-- in a 'NP' product instead of a tuple.
instance
  MulticurryableF (IsFunction curried) items a curried =>
  Multicurryable (->) items a curried
  where
  type UncurriedArgs (->) = NP I
  multiuncurry = multiuncurryF @(IsFunction curried)
  multicurry = multicurryF @(IsFunction curried)

class
  MulticurryableF (b :: Bool) items a curried
    | items a -> curried,
      b curried -> items a 
  where
  multiuncurryF :: curried -> NP I items -> a
  multicurryF :: (NP I items -> a) -> curried

instance MulticurryableF 'False '[] a a where
  multiuncurryF a Nil = a
  multicurryF f = f Nil

instance
  MulticurryableF (IsFunction curried) rest tip curried =>
  MulticurryableF 'True (i ': rest) tip (i -> curried)
  where
  multiuncurryF f (I x :* rest) =
    multiuncurryF @(IsFunction curried) @rest @tip @curried (f x) rest
  multicurryF f i =
    multicurryF @(IsFunction curried) @rest @tip @curried $ \rest -> f (I i :* rest)

-- Instance for Either

type family IsEither f :: Bool where
  IsEither (Either _ _) = 'True
  IsEither _ = 'False


-- | The instance for 'Either' takes a sequence nested 'Either's, separates the
-- errors from the success value at the right tip, and stores any occurring
-- errors in a 'NS' sum.
instance
  MulticurryableE (IsEither curried) items a curried =>
  Multicurryable Either items a curried
  where
  type UncurriedArgs Either = NS I
  multiuncurry = multiuncurryE @(IsEither curried)
  multicurry = multicurryE @(IsEither curried)

class
  MulticurryableE (b :: Bool) items a curried
    | items a -> curried,
      b curried -> items a
  where
  multiuncurryE :: curried -> Either (NS I items) a
  multicurryE :: Either (NS I items) a -> curried

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
    Left x -> Left (Z (I x))
    Right rest ->
      case multiuncurryE @(IsEither curried) @rest @tip @curried rest of
        Left ns -> Left (S ns)
        Right x -> Right x
  multicurryE = \case
    Left rest -> case rest of
      Z (I x) -> Left x
      S rest' -> Right $ multicurryE @(IsEither curried) @_ @tip @curried (Left rest')
    Right tip -> Right $ multicurryE @(IsEither curried) @_ @tip @curried (Right tip)

-- $setup
-- >>> :set -XAllowAmbiguousTypes
-- >>> :set -XDataKinds 
-- >>> :set -XFunctionalDependencies 
-- >>> :set -XLambdaCase 
-- >>> :set -XTypeFamilies
-- >>> :set -XTypeOperators
-- >>> :set -XUndecidableInstances 
-- >>> import Multicurryable