module CostasLikeArrays.A320573 where
import Helpers.CostasLikeArrays (distinctDistances)
import Data.List (permutations)

a320573 :: Int -> Int
a320573 n = countMax distinctDistances $ permutations [0..n-1]

countMax :: (a -> Int) -> [a] -> Int
countMax _ []     = 0
countMax f (a:as) = recurse 1 (f a) as where
  recurse count _        []     = count
  recurse count knownMax (x:xs)
    | f x == knownMax = recurse (count + 1) knownMax xs
    | f x < knownMax  = recurse       count knownMax xs
    | otherwise       = recurse           1    (f x) xs
