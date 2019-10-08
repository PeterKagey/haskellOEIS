module Helpers.Subsets (oneIndexed, zeroIndexed, a048793_tabf, allSubsets, allSubsets', allSubsets'') where
import Data.Bits ((.&.))

a048793_tabf :: [[Integer]]
a048793_tabf = map (reverse . oneIndexed) [0..]

oneIndexed :: Integer -> [Integer]
oneIndexed n = map (1 +) (zeroIndexed n)

zeroIndexed :: Integer -> [Integer]
zeroIndexed n = count n 0 where
  count 0 _ = []
  count m c = count (m `div` 2) (c + 1) ++ take (fromIntegral $ m .&. 1) [c]

allSubsets :: [a] -> [[a]]
allSubsets = foldr nextSubsets [[]] where
  nextSubsets a existingSubsets = existingSubsets ++ map (a:) existingSubsets

-- This is intended to be used only when the argument is infinite.
allSubsets' :: [a] -> [[a]]
allSubsets' as = map (`subsetsBy` as) [0..]

-- This is intended to be used only when the argument is finite.
allSubsets'' :: [a] -> [[a]]
allSubsets'' as = map (`subsetsBy` as) [0..2^length as - 1]

-- Use the binary expansion of n to choose elements of the subset
subsetsBy :: Int -> [a] -> [a]
subsetsBy n [] = []
subsetsBy 0 _  = []
subsetsBy n (a:as)
  | even n =     subsetsBy (n `div` 2) as
  | odd n  = a : subsetsBy (n `div` 2) as


isProductFree as = [] == filter (`elem` as) quotients where
  quotients = map (\(a,a') -> a `div` a') $ filter (\(a,a') -> a `mod` a' == 0) $ filter (\(a,a') -> a /= a') [(a, a') | a <- as, a' <- as]
