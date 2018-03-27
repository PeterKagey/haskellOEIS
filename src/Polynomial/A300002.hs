module Polynomial.A300002 (a300002, a300002_list) where
import Helpers.Polynomial (validTerms)
import Data.List (genericTake)

a300002 :: Int -> Integer
a300002 n = a300002_list !! (n - 1)

-- This is quite inefficient
-- (1) It recomputes the `interpolate` function each time instead of caching the
--   results
-- (2) It stores the illegalTerms as a list instead of as a set
-- (3) It compares against interpolated functions that are clearly negative
a300002_list :: [Integer]
a300002_list = map (+1) polynomialSequence where
  polynomialSequence = 0 : recurse 1 where
    recurse k = nextTerm : recurse (k + 1) where
      nextTerm = head (validTerms points) where
        points = genericTake k polynomialSequence
