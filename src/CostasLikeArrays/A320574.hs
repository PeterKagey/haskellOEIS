module CostasLikeArrays.A320574 where
import Helpers.CostasLikeArrays (distinctDistances, countPermutationsUpToDihedralSymmetry)
import Data.List (permutations)

a320574 :: Int -> Int
a320574 n = countPermutationsUpToDihedralSymmetry n $ allMax distinctDistances $ permutations [0..n-1]

allMax :: (a -> Int) -> [a] -> [a]
allMax _ []     = []
allMax f (a:as) = recurse [a] (f a) as where
  recurse known _ []  = known
  recurse known knownMax (x:xs)
    | f x == knownMax = recurse (x : known) knownMax xs
    | f x < knownMax  = recurse       known knownMax xs
    | otherwise       = recurse         [x]    (f x) xs
