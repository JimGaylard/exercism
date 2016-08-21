module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform x = fromList $ scoresIn $ toList x where
  scoresIn = foldr ((++) . scoreIn) []
  scoreIn (i, ys) = zip (map toLower ys) (repeat i)
