module Helpers.Binary (lastBits) where

-- Number of partitions of the binary expansion of n into consecutive blocks with no leading zeroes.
lastBits :: Int -> [Int]
lastBits n = recurse 0 n where
  recurse _ 0 = []
  recurse k n' = (n `mod` 2^k) : recurse (k + 1) (div n' 2)
