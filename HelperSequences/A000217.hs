module HelperSequences.A000217 (a000217) where

  a000217 :: Int -> Integer
  a000217 n = m * (m + 1) `div` 2 where
    m = toInteger n
