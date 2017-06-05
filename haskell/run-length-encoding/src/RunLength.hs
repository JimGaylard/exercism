module RunLength (decode, encode) where

import Data.Char (isDigit)


decode :: String -> String
decode [] = ""
decode xs = loop xs []  where
  loop :: String -> String -> String
  loop [] _ = []
  loop (y:ys) ns = if isDigit y then loop ys (y : ns)
                                else printNums ns y
                                  ++ loop ys []
  printNums [] char = [char]
  printNums ns char =
    replicate (read $ reverse ns :: Int) char

encode :: String -> String
encode [] = ""
encode (x:xs) = loop xs x 1 where
  loop :: String -> Char -> Int -> String
  loop [] y acc = numberFor acc ++ [y]
  loop (z:zs) y acc = if z == y then loop zs y (acc +1)
                                else numberFor acc
                                  ++ y
                                  : loop zs z 1
  numberFor n = if n == 1 then ""
                          else show n
