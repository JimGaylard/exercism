module DNA (count, nucleotideCounts) where

import qualified Data.Map as M

type Nucleotide = Char
type Strand = String
type ErrorString = String
type Count = Int

nucleotides :: [Char]
nucleotides = "ACGT"

count :: Nucleotide -> Strand -> Either ErrorString Count
count x xs
  | notAllNucleotides (x:xs) = Left $ "invalid nucleotide '" ++ badNucleotide (x:xs) : "'"
  | otherwise = Right $ count' x xs
    where count' x' xs' = length $ filter (==x') xs'

notAllNucleotides :: Strand -> Bool
notAllNucleotides = any notNucleotide

badNucleotide :: Strand -> Char
badNucleotide = head . filter notNucleotide

notNucleotide :: Nucleotide -> Bool
notNucleotide x = not $ elem x nucleotides

nucleotideCounts :: String -> Either String (M.Map Char Int)
nucleotideCounts xs = fmap count xs
