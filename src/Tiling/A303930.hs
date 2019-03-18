module Tiling.A303930 where

-- Count leaf-free subgraphs of the 2 X n grid up to horizontal and vertical flipping.
-- https://oeis.org/A020876

-- Ten for n = 4:
-- +   +   +   +  ┌───┐   +   +  +   ┌───┐   +  ┌───┐   ┌───┐  ┌───────┐   +  ┌───────┬───┐  ┌───────────┐  ┌───┬───┬───┐  ┌───┬───┬───┐  ┌───┬───┐   +
--                │   │              │   │      │   │   │   │  │       │      │       │   │  │           │  │   │   │   │  │   │   │   │  │   │   │
-- +   +   +   +, └───┘   +   +, +   └───┘   +, └───┘   └───┘, └───────┘   +, └───────┴───┘, └───────────┘, └───┴───┴───┘, └───┘   └───┘, └───┴───┘   +.

a303930 :: Int -> Integer
a303930 n = a303930_list !! (n - 1)

-- A303930
a303930_list :: [Integer]
a303930_list = map (`div` 4) sumOfOrbits where
  sumOfOrbits = foldr (zipWith (+)) allCount [rotationalCount, horizontalCount, verticalCount]

-- A093129
allCount :: [Integer]
allCount = 1 : 2 : recurse 1 2 where
  recurse a b = nextTerm : recurse b nextTerm where
    nextTerm = 5 * (b - a)

-- A001519
verticalCount :: [Integer]
verticalCount = 1 : 2 : recurse 1 2 where
  recurse a b = nextTerm : recurse b nextTerm where
    nextTerm = 3 * b - a

horizontalCount :: [Integer]
horizontalCount = 1 : 2 : 3 : 7 : recurse 1 2 3 7 where
  recurse a b c d = nextTerm : recurse b c d nextTerm where
    nextTerm = 5 * (c - a)

rotationalCount :: [Integer]
rotationalCount = 1 : 2 : 3 : 5 : recurse 1 2 3 5 where
  recurse a b c d = nextTerm : recurse b c d nextTerm where
    nextTerm = 5 * (c - a)
