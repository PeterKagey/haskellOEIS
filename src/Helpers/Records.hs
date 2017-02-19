module Helpers.Records (maxIndices) where

maxIndices :: Integral a => [a] -> [Int]
maxIndices [] = []
maxIndices (a:as) = 0 : remainder (zip [1..] as) a where
  remainder [] _ = []
  remainder ((i, a_i) : remainingPairs) maxTerm
    | a_i > maxTerm = i : remainder remainingPairs a_i
    | otherwise     = remainder remainingPairs maxTerm
