{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Multicurryable

type Fun0 = Int
type Fun0b = IO Int
type Fun1 = Bool -> Int
type Fun1b = Bool -> IO Int
type Fun2 = Char -> Bool -> Int
type Fun2b = Char -> Bool -> IO Int

-- fun0 = multiuncurry @(->)


main :: IO ()
main = putStrLn "Test suite not yet implemented."
