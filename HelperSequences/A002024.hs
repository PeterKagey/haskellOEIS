module HelperSequences.A002024 (a002024) where

-- This is a slow proof-of-concept. This can be computed with less complexity as:
-- floor(sqrt(2n) + 1/2).

a002024 :: Int -> Int
a002024 n = a002024List !! (n - 1)

a002024List :: [Int]
a002024List = remaining 1 where
  remaining n = (take n $ repeat n) ++ remaining (n + 1)
