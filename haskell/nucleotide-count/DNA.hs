module DNA (count, nucleotideCounts) where

import qualified Data.Map as M

nucleotides :: String
nucleotides = "ACGT"

count :: Char -> String -> Either String Int
count x xs
  | notAllNucleotides (x:xs) = Left $ "invalid nucleotide '" ++ badNucleotide (x:xs) : "'"
  | otherwise = Right $ count' x xs

count' :: Char -> String -> Int
count' x xs = length $ filter (==x) xs

notAllNucleotides :: String -> Bool
notAllNucleotides = any notNucleotide

badNucleotide :: String -> Char
badNucleotide = head . filter notNucleotide

notNucleotide :: Char -> Bool
notNucleotide = (`notElem` nucleotides)

nucMap :: M.Map Char Int
nucMap = M.fromList $ zip nucleotides $ repeat 0

nucleotideCounts :: String -> Either String (M.Map Char Int)
nucleotideCounts xs
  | notAllNucleotides xs = Left $ "invalid nucleotide '" ++ badNucleotide xs : "'"
  | otherwise = Right $ foldr (\x y -> M.insert x (count' x xs) y) nucMap xs
