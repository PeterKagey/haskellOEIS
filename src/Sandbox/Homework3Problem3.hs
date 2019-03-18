module Miscellaneous.Homework3Problem3 where
import Data.List (findIndex, permutations, sort)

allPermutations :: Int -> [[Int]]
allPermutations n = sort $ permutations [1..n]

-- minDescents :: Int -> [Int]
minDescents n = map minDescents $ allPermutations n where
  minDescents permutation = case firstIndex of
    Nothing -> 0
    Just k -> k + 1
    where
    firstIndex = findIndex (uncurry (>)) $ zip permutation (tail permutation)

list :: [Integer]
list = recurse 1 0 where
  recurse index lastTerm = lastTerm : recurse (index + 1) newTerm where
    newTerm = lastTerm * (index + 1) + index*index

factorial n = product [1..n]

a 1 = 0
a n = sum $ map (f n) [0..n-1]

f :: Int -> Int -> Int
f n k = factorial (n - 1) + a(n - 1) - s where
  s = sum $ map (f (n-1)) [0..k-1]
