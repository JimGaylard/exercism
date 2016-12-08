module Triangle (TriangleType(..), triangleType) where

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as M

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | invalid a b c    = Illegal
  | commonSides == 1 = Equilateral
  | commonSides == 2 = Isosceles
  | commonSides == 3 = Scalene
  | otherwise        = Illegal where
    commonSides = M.distinctSize $ M.fromList [a, b, c]
    invalid x y z =
         (x + y <= z)
      || (x + z <= y)
      || (y + z <= x)
