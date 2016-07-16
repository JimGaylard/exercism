module Phone (areaCode, number, prettyPrint) where

import Data.Char (isDigit)

number :: String -> Maybe String
number x =
  let clean = filter isDigit x in
      case length clean of
        10 -> Just clean
        11 -> if head clean == '1'
                        then Just (tail clean)
                        else Nothing
        _  -> Nothing

areaCode :: String -> Maybe String
areaCode x = take 3 <$> number x

formattedNumber :: String -> Maybe String
formattedNumber x = (\y -> f y ++ "-" ++ l y) <$> number x
  where f = take 3 . drop 3
        l = reverse . take 4 . reverse

prettyPrint :: String -> Maybe String
prettyPrint x = do
  num <- number x
  area <- areaCode num
  formatted <- formattedNumber num
  Just ("(" ++ area ++ ")" ++ " " ++ formatted)
