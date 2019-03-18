module CostasLikeArrays.A320575 where
import Helpers.CostasLikeArrays (distinctDistances)
import Data.List (permutations)

a320575 :: Int -> Int
a320575 n = countMin distinctDistances $ permutations [0..n-1]

countMin :: (a -> Int) -> [a] -> Int
countMin _ []     = 0
countMin f (a:as) = recurse 1 (f a) as where
  recurse count _        []     = count
  recurse count knownMax (x:xs)
    | f x == knownMax = recurse (count + 1) knownMax xs
    | f x > knownMax  = recurse       count knownMax xs
    | otherwise       = recurse           1    (f x) xs
