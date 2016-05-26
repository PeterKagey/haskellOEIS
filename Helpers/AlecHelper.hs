module Helpers.AlecHelper (buildAlecSequence) where

buildAlecSequence :: Integral a => ([a] -> [Int]) -> ([Int] -> a) -> [a] -> [a]
buildAlecSequence matchingIndices reducer seed = alecSequence where
  alecSequence = seed ++ remainder (length seed) where
    remainder len = a_i : remainder (len + 1) where
      a_i = reducer $ matchingIndices $ take len alecSequence
