module DNA where

toRNA :: String -> String
toRNA = map toRNA'
  where toRNA' x = case x of
          'G' -> 'C'
          'C' -> 'G'
          'A' -> 'U'
          'T' -> 'A'
          _   -> error (x : ": unknown nucleotide")

