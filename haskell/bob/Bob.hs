{-# LANGUAGE OverloadedStrings #-}
module Bob (responseFor) where
import Data.Char (toUpper)
import qualified Data.Text as T (length, strip, pack)
import Text.Regex.Posix ((=~))

responseFor :: String -> String
responseFor phrase
  | T.length (T.strip $ T.pack phrase) == 0 = "Fine. Be that way!"
  | shouting phrase = "Whoa, chill out!"
  | head (reverse phrase) == '?' = "Sure."
  | otherwise = "Whatever."
  where shouting p = map toUpper p == p &&
          p =~ ("[A-Z]" :: String) :: Bool
