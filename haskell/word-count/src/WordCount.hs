module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.List.Split (wordsBy)

import qualified Data.Map as M

wordCount :: String -> M.Map String Int
wordCount s = M.fromListWith (+) $ zip (realWords s) (repeat 1)

realWords :: String -> [String]
realWords s = wordsBy (not . isAlphaNum) $ map toLower s
