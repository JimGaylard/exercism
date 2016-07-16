module Hamming (distance) where

distance :: String -> String -> Int
distance x y = length . filter id $ zipWith (/=) x y
