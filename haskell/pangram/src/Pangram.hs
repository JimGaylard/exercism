module Pangram (isPangram) where

import Data.Char (isAlpha)
import Data.Set (fromList)

isPangram :: String -> Bool
isPangram text = length (fromList $ alphaOnly text) == 26 where
  alphaOnly = filter isAlpha
