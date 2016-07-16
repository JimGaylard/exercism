module SumOfMultiples (sumOfMultiples) where
import Data.List (union)

sumOfMultiples :: Integral a => [a] -> a -> a
sumOfMultiples nums y = sum (uniqueMultiples nums y)

uniqueMultiples :: Integral a => [a] -> a -> [a]
uniqueMultiples [] _ = []
uniqueMultiples (x:xs) y = union (filter multiple [1..y-1]) (uniqueMultiples xs y)
  where multiple = (\z -> (mod z x) == 0)
