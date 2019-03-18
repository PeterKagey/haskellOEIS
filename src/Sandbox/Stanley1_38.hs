module Miscellaneous.Stanley1_38 where
import Data.List ((\\), permutations)

type Permutation = (Int, Int -> Int)
type CycleStructure = [[Int]]

allPermutations :: Int -> [Permutation]
allPermutations n = map (\p -> (n, p)) $ map (!!) $ permutations [0..n-1]

cycleStructure :: Permutation -> CycleStructure
cycleStructure (n, p) = reverse $ recurse (reverse [0..n-1]) where
  recurse :: [Int] -> CycleStructure
  recurse [] = []
  recurse (i:is) = newCycle : recurse (is \\ newCycle) where
    newCycle = newCycleContaining i (n, p)

newCycleContaining :: Int -> Permutation -> [Int]
newCycleContaining i (_, permutation) = i : recurse i where
  recurse k
    | i == p_k = []
    | otherwise          = p_k : recurse p_k where
      p_k = permutation k

fundamentalTransformation :: Permutation -> Permutation
fundamentalTransformation permutation@(n, p) = (n, newPermutation) where
  newPermutation = (!!) $ concat $ cycleStructure permutation
