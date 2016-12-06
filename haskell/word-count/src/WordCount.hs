module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import qualified Data.MultiSet as MultiSet
import Data.List.Split (wordsBy)

import qualified Data.Map as M

wordCount :: String -> M.Map String Int
wordCount s = MultiSet.toMap $ MultiSet.fromList (realWords s)

realWords :: String -> [String]
realWords s = wordsBy (not . isAlphaNum) $ map toLower s
