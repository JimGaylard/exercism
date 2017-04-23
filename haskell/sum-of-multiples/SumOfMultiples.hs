module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: Integral a => [a] -> a -> a
sumOfMultiples xs limit =
  sum $ nub $ multiple <$> nats <*> xs
        where multiple x y = if x `mod` y == 0
                                then x
                                else 0
              nats = [1..limit - 1]
