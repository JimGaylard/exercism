module Acronym (abbreviate) where

import Data.Char (toUpper)

abbreviate :: String -> String
abbreviate xs = toUpper . head <$> words xs
