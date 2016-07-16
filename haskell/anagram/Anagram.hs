{-# LANGUAGE OverloadedStrings #-}

module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor w ws =
  filter (\x -> sortedLower x == sortedLower w && lower x /= lower w) ws
    where sortedLower =  sort . map toLower
          lower = map toLower
