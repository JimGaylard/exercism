module Sublist where

data Sublist = Equal | Sublist | Superlist | Unequal deriving (Show, Eq)

sublist :: Eq a => [a] -> [a] -> Sublist
sublist x y
  | sizex < sizey = if subset x y then Sublist else Unequal
  | sizex > sizey = if subset y x then Superlist else Unequal
  | otherwise = if subset x y then Equal else Unequal
  where sizex = length x
        sizey = length y

subset :: Eq a => [a] -> [a] -> Bool
subset [] [] = True
subset _ [] = False
subset [] _ = True
subset x y =
  if x == take (length x) y
     then True
     else subset x (drop 1 y)
