module Helpers.ListHelpers (partialSums) where

partialSums :: Num a => [a] -> [a]
partialSums = sums 0 where
  sums _ []     = []
  sums i (x:xs) = x' : sums x' xs where
    x' = i + x
