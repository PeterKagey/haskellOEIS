module CostasLikeArrays.A321534 where
import Helpers.CostasLikeArrays (distinctDirections, countPermutationsUpToDihedralSymmetry)
import Data.List (permutations)

a321534 :: Int -> Int
a321534 n = countPermutationsUpToDihedralSymmetry n $ allMax distinctDirections $ permutations [0..n-1]

allMax :: (a -> Int) -> [a] -> [a]
allMax _ []     = []
allMax f (a:as) = recurse [a] (f a) as where
  recurse known _ []  = known
  recurse known knownMax (x:xs)
    | f x == knownMax = recurse (x : known) knownMax xs
    | f x < knownMax  = recurse       known knownMax xs
    | otherwise       = recurse         [x]    (f x) xs
