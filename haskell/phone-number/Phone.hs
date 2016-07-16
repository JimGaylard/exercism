module Phone (areaCode, number, prettyPrint) where

number :: String -> Maybe Integer
number x
  | (length x) == 10 = Just (read x :: Integer)
  | (length x) == 11 = validateFirst x
  | (length x) > 11 || (length x) < 10 = Nothing
    where validateFirst = undefined

areaCode :: Integer -> Maybe Integer
areaCode = undefined

prettyPrint :: Integer -> String
prettyPrint = undefined

