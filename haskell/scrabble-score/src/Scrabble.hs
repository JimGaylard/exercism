module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toLower)
import Data.Map as M (Map, fromList, toList)
import Data.Map.Lazy as ML (findWithDefault)
import Data.Maybe

scores :: Map Int String
scores =
  fromList [
               (1, ['A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T'])
             , (2, ['D', 'G'])
             , (3, ['B', 'C', 'M', 'P'])
             , (4, ['F', 'H', 'V', 'W', 'Y'])
             , (5, ['K'])
             , (8, ['J', 'X'])
             , (10, ['Q', 'Z'])
             ]

scoreLetter :: Char -> Int
scoreLetter a = ML.findWithDefault 0 (toLower a) $ transform scores where
  transform = fromList . scoresIn . toList where
    scoresIn = foldr ((++) . scoreIn) []
    scoreIn :: (Int, String) -> [(Char, Int)]
    scoreIn (i, ys) = zip (map toLower ys) (repeat i)

scoreWord :: String -> Int
scoreWord word = sum $ fmap scoreLetter word
