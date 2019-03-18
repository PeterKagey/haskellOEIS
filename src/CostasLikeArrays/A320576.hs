module CostasLikeArrays.A320576 where
import Helpers.CostasLikeArrays (distinctDistances, countPermutationsUpToDihedralSymmetry)
import Data.List (permutations)

a320576 :: Int -> Int
a320576 n = countPermutationsUpToDihedralSymmetry n $ allMin distinctDistances $ permutations [0..n-1]

allMin :: (a -> Int) -> [a] -> [a]
allMin _ []     = []
allMin f (a:as) = recurse [a] (f a) as where
  recurse known _ []  = known
  recurse known knownMax (x:xs)
    | f x == knownMax = recurse (x : known) knownMax xs
    | f x > knownMax  = recurse       known knownMax xs
    | otherwise       = recurse         [x]    (f x) xs
