module Sandbox.RedAndBlueDice where
import Data.List (permutations, tails)
-- https://math.stackexchange.com/questions/3035452/error-in-solution-of-peter-winkler-red-and-blue-dice-puzzle
-- On permutations

equalSumRootedStrings :: [Int] -> [Int] -> Int
equalSumRootedStrings [] _ = 0
equalSumRootedStrings _ [] = 0
equalSumRootedStrings (x:xs) (y:ys) = recurse xs ys x y 0 where
  recurse [] [] sumA sumB c
    | sumA == sumB = c + 1
    | otherwise    = c
  recurse (a:as) [] sumA sumB c
    | sumA < sumB = recurse as [] (a + sumA) sumB c
    | sumA == sumB = c + 1
    | sumA > sumB = c
  recurse [] (b:bs) sumA sumB c
    | sumA < sumB = c
    | sumA == sumB = c + 1
    | sumA > sumB = recurse [] bs sumA (b + sumB) c
  recurse (a:as) (b:bs) sumA sumB c
    | sumA == sumB = recurse as (b:bs) (a + sumA) sumB (c + 1)
    | sumA < sumB = recurse as (b:bs) (a + sumA) sumB c
    | sumA > sumB = recurse (a:as) bs sumA (b + sumB) c

equalSumSubstrings :: [Int] -> [Int] -> Int
equalSumSubstrings s1 s2 = sum [equalSumRootedStrings a b | a <- tails s1, b <- tails s2]

permutationPairs n = [equalSumSubstrings a b | a <- permutations [1..n], b <- permutations [1..n]]

-- Fewest number of nonempty substrings in a, b in S_n with equal sums.
-- 1,3,6,8,11,15
