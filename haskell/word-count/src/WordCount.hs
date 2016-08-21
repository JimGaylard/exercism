module WordCount (wordCount) where

import Data.Char (toLower)

import qualified Data.Map as M

wordCount :: String -> M.Map String Int
wordCount s = M.fromListWith (+) $ zip (realWords s) (repeat 1)

realWords :: String -> [String]
realWords s = words $ map toLower (replace s)

replace :: String -> String
replace = map (\c -> if isOk c then c else ' ')

isOk :: Char -> Bool
isOk c = c `notElem` "_#&@$%^!?:'\",."
