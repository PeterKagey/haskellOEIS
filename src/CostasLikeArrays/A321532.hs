module CostasLikeArrays.A321532 where
import Helpers.CostasLikeArrays (distinctDirections)
import Data.List (permutations)

a321532 :: Int -> Int
a321532 n = countMax distinctDirections $ permutations [0..n-1]

countMax :: (a -> Int) -> [a] -> Int
countMax _ []     = 0
countMax f (a:as) = recurse 1 (f a) as where
  recurse count _        []     = count
  recurse count knownMax (x:xs)
    | f x == knownMax = recurse (count + 1) knownMax xs
    | f x < knownMax  = recurse       count knownMax xs
    | otherwise       = recurse           1    (f x) xs
