module HelperSequences.A070939 (a070939) where

a070939 :: Int -> Int
a070939 n
  | n < 2     = 1
  | otherwise = a070939 (n `div` 2) + 1
