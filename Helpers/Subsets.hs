module Helpers.Subsets (oneIndexed, zeroIndexed) where

oneIndexed :: Integer -> [Integer]
oneIndexed n = map (1 +) (zeroIndexed n)

zeroIndexed :: Integer -> [Integer]
zeroIndexed n = recurse n 0 [] where
  recurse 0 c accum = accum
  recurse n c accum = if odd n then recurse (n `div` 2) (c + 1) (c : accum) else recurse (n `div` 2) (c + 1) accum where

