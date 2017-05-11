module Scrabble (scoreLetter, scoreWord) where

import Data.Map(Map, fromList)

scores :: Map Char Int
scores = fromList $
  [
  , [1, ['A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T']
  , [2, ['D', 'G']
  , [3, ['B', 'C', 'M', 'P']]
  , [4, ['F', 'H', 'V', 'W', 'Y']]
  , [5, ['K']]
  , [8, ['J', 'X']]
  , [10, ['Q', 'Z']]
  ]

scoreLetter = undefined

scoreWord = undefined


-- Letter                           Value
-- A, E, I, O, U, L, N, R, S, T       1
-- D, G                               2
-- B, C, M, P                         3
-- F, H, V, W, Y                      4
-- K                                  5
-- J, X                               8
-- Q, Z                               10
