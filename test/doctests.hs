module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest [
    "-ilib", 
    "lib/Multicurryable.hs" 
    ]