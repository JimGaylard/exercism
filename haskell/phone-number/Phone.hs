module Phone (areaCode, number, prettyPrint) where

import Data.Char (isDigit)

number :: String -> Maybe String
number x
  | (length $ digits x) == 10 = Just (digits x)
  | (length $ digits x) == 11 = validateFirst x
  | otherwise = Nothing
    where validateFirst y
            | head y == '1' = Just (drop 1 y)
            | otherwise = Nothing
          digits = filter isDigit

areaCode :: String -> Maybe String
areaCode x = case number x of
               Nothing -> Nothing
               Just y -> Just (take 3 y)

formattedNumber :: String -> Maybe String
formattedNumber x = case number x of
               Nothing -> Nothing
               Just y -> Just $ formattedNumber' y
                 where formattedNumber' z = take 3 (drop 3 z) ++ "-" ++ drop 6 z

prettyPrint :: String -> Maybe String
prettyPrint x = do
  num <- number x
  area <- areaCode num
  formatted <- formattedNumber num
  Just ("(" ++ area ++ ")" ++ " " ++ formatted)
