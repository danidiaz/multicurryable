{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Multicurryable
import Data.SOP.NP
import Data.SOP.NS
import Data.Functor.Identity

type Fun0 = Int
type Fun0b = IO Int
type Fun1 = Bool -> Int
type Fun1b = Bool -> IO Int
type Fun2 = Char -> Bool -> Int
type Fun2b = Char -> Bool -> IO Int

-- Signatures omitted to check type inference
ufun0 = multiuncurry @(->) @_ @_ @Fun0 5
ufun0b = multiuncurry @(->) @_ @_ @Fun0b (pure 5)
ufun1 = multiuncurry @(->) @_ @_ @Fun1 \_ -> 5
ufun1b = multiuncurry @(->) @_ @_ @Fun1b \_ -> pure 5
ufun2 = multiuncurry @(->) @_ @_ @Fun2 \_ _ -> 5
ufun2b = multiuncurry @(->) @_ @_ @Fun2b \_ _ -> pure 5

fun0 = multicurry @(->) @_ @_ ufun0 
fun0b = multicurry @(->) @_ @_ ufun0b
fun1 = multicurry @(->) @_ @_ ufun1 
fun1b = multicurry @(->) @_ @_ ufun1b
fun2 = multicurry @(->) @_ @_ ufun2
fun2b = multicurry @(->) @_ @_ ufun2b


type Eith0 = Int
type Eith1 = Either Bool Int
type Eith2 = Either Char (Either Bool Int)

ueith0 :: Either (NS I '[]) Int
ueith0 = multiuncurry @Either @_ @_ @Eith0 5
ueith1 :: Either (NS I '[Bool]) Int
ueith1 = multiuncurry @Either @_ @_ @Eith1 $ Right 5
ueith2 :: Either (NS I '[Char, Bool]) Int
ueith2 = multiuncurry @Either @_ @_ @Eith2 $ Right (Right 5)

eith0 = multicurry @Either @_ @_ ueith0
eith1 = multicurry @Either @_ @_ ueith1
eith2 = multicurry @Either @_ @_ ueith2

main :: IO ()
main = putStrLn "Test suite not yet implemented."
