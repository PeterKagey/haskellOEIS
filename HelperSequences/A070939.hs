module HelperSequences.A070939 (a070939) where

  a070939 :: Int -> Int
  a070939 n = if n < 2 then 1 else a070939 (n `div` 2) + 1
