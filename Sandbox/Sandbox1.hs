module Sandbox.Sandbox1 (a) where
import Data.Ratio (denominator)
import Helpers.Primes (isPrime)
import Helpers.ListHelpers (reciprocalSum)

-- starting with a list containing only n, append the reciprocal of the
-- denominator of the sum of the reciprocals of the list repeatedly until the
-- denominator equals 1
-- Let a n be the length of such a list.
-- e.g. a(12) = 5.
-- [12] -> 1/12
-- [12, 12]          -> 1/12 + 1/12 = 1/6
-- [12, 12, 6]       -> 1/12 + 1/12 + 1/6 = 1/3
-- [12, 12, 6, 3]    -> 1/12 + 1/12 + 1/6 + 1/3 = 1/3 = 2/3
-- [12, 12, 6, 3, 3] -> 1/12 + 1/12 + 1/6 + 1/3 = 1/3 = 1/1

a :: Integer -> Int
a n = loop [n] where
  loop list = if s == 1 then length list else loop (s : list) where
    s = denominator $ reciprocalSum list
