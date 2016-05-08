module Helpers.AlecHelper (buildAlecSequence) where

buildAlecSequence :: ([Integer] -> [Int]) -> ([Integer] -> Integer) -> [Integer] -> [Integer]
buildAlecSequence matchingIndices reducer seed = alecSequence where
  alecSequence = seed ++ (remainder (length seed)) where
    remainder len = a_i : remainder (len + 1) where
      a_i = reducer $ map toInteger $ matchingIndices $ take len alecSequence
