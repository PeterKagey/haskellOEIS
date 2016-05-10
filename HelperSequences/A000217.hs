module HelperSequences.A000217 (a000217) where

a000217 :: Integer -> Integer
a000217 n = n * (n + 1) `div` 2
