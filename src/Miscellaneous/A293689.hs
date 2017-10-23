module Miscellaneous.A293689 (a293689, a293689_list) where
import Helpers.Primes (isPrime)
-- The Prime Ant

-- [3 4 5 6 ...]
-- [2]

-- [4 5 6 ...]
-- [3 2]

-- [5 6 7 ...]
-- [4 3 2]

-- [2 5 6 7 ...]
-- [5 2]

a293689 = (!!) a293689_list

a293689_list :: [Int]
a293689_list = scanl (+) 0 firstDifferences where
  firstDifferences = recurse [3..] [2] where
    recurse (f:fs) (c:cs)
      | isPrime c = 1 : recurse fs (f:c:cs)
      | otherwise = (-1) : recurse newFs newCs where
        d = leastPrimeDivisor c
        newCs = case cs of (h:t) -> h + d : t
        newFs = c `div` d : f : fs

-- least prime divisor
leastPrimeDivisor n = head $ filter (\i -> n `mod` i == 0) [2..]
