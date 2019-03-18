module Miscellaneous.Problem70 where

-- The idea is to count the number of ways to partition the 1 x n grid into
-- triangles with all vertices on gridpoints.
-- Is this https://oeis.org/A048990?
lowerBound :: Integer -> Integer
lowerBound 1 = 2
lowerBound n = 2^((2*n)-1) + 3 * div (sum (map (\k -> lowerBound k * lowerBound (n-k)) [1..n-1])) 2
