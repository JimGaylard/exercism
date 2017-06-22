module Acronym (abbreviate) where

import Control.Applicative ((<|>))
import Data.Char (toUpper, isAlpha, isAsciiLower, isAsciiUpper)
import Text.ParserCombinators.ReadP (ReadP, eof, many, option, satisfy)

abbreviate :: String -> String
abbreviate xs = toUpper . head <$> words xs

acroParser :: ReadP [String]
acroParser = many $ do
  word <- normalWord
  _ <- whiteSpace
  return word

myWord :: ReadP [String]
myWord = camelWord <|> acronymWord

normalWord :: ReadP String
normalWord = do
  first <- letter
  rest <- many $ satisfy isAsciiLower
  return $ first : rest

camelWord :: ReadP [String]
camelWord = do
  first <- letter
  lower <- many $ satisfy isAsciiLower
  upper <- satisfy isAsciiUpper
  rest <- many $ satisfy isAsciiLower
  return (first : lower) ++ (upper : rest)

acronymWord :: ReadP String
acronymWord = undefined

letter :: ReadP Char
letter =
  satisfy isAsciiUpper <|> satisfy isAsciiLower

whiteSpace :: ReadP Char
whiteSpace = satisfy (\c -> any (\ch -> c == ch) " \n\r")
