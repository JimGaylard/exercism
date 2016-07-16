module Grains (square, total) where

total :: Integer
total = sum $ map square chessboard
  where chessboard = [1..64]

square :: Integer -> Integer
square x = 2^(x-1)
