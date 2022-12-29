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


main :: IO ()
main = putStrLn "Test suite not yet implemented."
