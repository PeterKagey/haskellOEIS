import Helpers.Primes (divisors)
import Data.Set (fromList, insert)
import Data.List (inits)

a328443_list :: [Integer]
a328443_list = 1 : 2 : recurse (fromList [1,2])  where
  recurse s = a_n : recurse (a_n `insert` s) where
    a_n = head $ filter (`notElem` s) initialDivisors where
      initialDivisors = concatMap (divisors . sum) $ tail $ inits a328443_list
