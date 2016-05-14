module Helpers.Subsets (oneIndexed, zeroIndexed) where

oneIndexed :: Integer -> [Integer]
oneIndexed n = map (1 +) (zeroIndexed n)

zeroIndexed :: Integer -> [Integer]
zeroIndexed n = count n 0 [] where
  count 0 c accum = accum
  count n c accum = if odd n then r (c : accum) else r accum where
    r = count (n `div` 2) (c + 1)
