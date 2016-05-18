module Helpers.Subsets (oneIndexed, zeroIndexed) where
import Data.Bits ((.&.), shiftR)

oneIndexed :: Integer -> [Integer]
oneIndexed n = map (1 +) (zeroIndexed n)

zeroIndexed :: Integer -> [Integer]
zeroIndexed n = count n 0 where
  count 0 _ = []
  count m c = count (m `div` 2) (c + 1) ++ take (fromIntegral $ m .&. 1) [c]

