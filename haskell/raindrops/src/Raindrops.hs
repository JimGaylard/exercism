module Raindrops (convert) where

convert :: (Integral a, Show a) => a -> String
convert n
  | factorOf 3 && factorOf 5 && factorOf 7 = "PlingPlangPlong"
  | factorOf 3 && factorOf 5 = "PlingPlang"
  | factorOf 3 && factorOf 7 = "PlingPlong"
  | factorOf 5 && factorOf 7 = "PlangPlong"
  | factorOf 3 = "Pling"
  | factorOf 5 = "Plang"
  | factorOf 7 = "Plong"
  | otherwise = show n where
    factorOf x = mod n x == 0
