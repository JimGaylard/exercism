module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toLower)
import Data.Map as M (Map, fromList, toList)
import Data.Map.Lazy as ML (findWithDefault)

scores :: Map Int String
scores =
  fromList [
               (1, "AEIOULNRST")
             , (2, "DG")
             , (3, "BCMP")
             , (4, "FHVWY")
             , (5, "K")
             , (8, "JX")
             , (10, "QZ")
             ]

scoreLetter :: Char -> Int
scoreLetter a = ML.findWithDefault 0 (toLower a) $ transform scores where
  transform = fromList . scoresIn . toList where
    scoresIn = foldr ((++) . scoreIn) []
    scoreIn :: (Int, String) -> [(Char, Int)]
    scoreIn (i, ys) = zip (map toLower ys) (repeat i)

scoreWord :: String -> Int
scoreWord word = sum $ fmap scoreLetter word
