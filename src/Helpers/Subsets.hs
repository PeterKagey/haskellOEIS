module Helpers.Subsets (oneIndexed, zeroIndexed, a048793_tabf, allSubsets) where
import Data.Bits ((.&.))

a048793_tabf :: [[Integer]]
a048793_tabf = map (reverse . oneIndexed) [1..]

oneIndexed :: Integer -> [Integer]
oneIndexed n = map (1 +) (zeroIndexed n)

zeroIndexed :: Integer -> [Integer]
zeroIndexed n = count n 0 where
  count 0 _ = []
  count m c = count (m `div` 2) (c + 1) ++ take (fromIntegral $ m .&. 1) [c]

allSubsets :: [a] -> [[a]]
allSubsets = foldr nextSubsets [[]] where
  nextSubsets a existingSubsets = existingSubsets ++ map (a:) existingSubsets
