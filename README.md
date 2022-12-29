# Multicurryable

If you have a value of type `Int -> Char -> Bool -> Float`, you can use `multiuncurry @(->)` to convert it to a value of type `NP Identity '[Int,Char,Bool] -> Float`, where [`NP`](https://hackage.haskell.org/package/sop-core-0.5.0.2/docs/Data-SOP-NP.html) is a n-ary product from sop-core. `multicurry @(->)` goes in the other direction.

If you have a value of type `Either Int (Either Char (Either Bool Float)`, you can use `multiuncurry @Either` to convert it to a value of type `Either (NS Identity '[Int,Char,Bool]) Float`, where [`NS`](https://hackage.haskell.org/package/sop-core-0.5.0.2/docs/Data-SOP-NS.html) is a n-ary sum from sop-core. `multicurry @Either` goes in the other direction.
