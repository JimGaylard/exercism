module Beer (song) where

beers :: [Integer]
beers = [0..99]

song :: String
song = foldr (\x a -> a ++ verse x) "" beers

verse :: Integer -> String
verse n
  | n == 0 = "No more bottles of beer on the wall, no more bottles of beer.\n"
      ++ "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
  | otherwise = pluralisation n
      ++ " of beer on the wall, "
      ++ pluralisation n
      ++ " of beer.\n"
      ++ "Take "
      ++ lastBeer n
      ++ " down and pass it around, "
      ++ pluralisation (n - 1)
      ++ " of beer on the wall.\n"
      ++ "\n" where
        pluralisation x
          | x == 0 = "no more bottles"
          | x == 1 = "1 bottle"
          | otherwise = show x ++ " bottles"
        lastBeer x
          | x == 1 = "it"
          | otherwise = "one"
