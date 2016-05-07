module Helpers.Primes (prime_factors, prime_powers, unique_prime_factors) where

import Data.List (group, nub, sort)

-- Taken from
-- http://stackoverflow.com/questions/21276844/prime-factors-in-haskell
-- will write my own implementation later (fingers crossed!)

prime_factors :: Int -> [Int]
prime_factors n =
  case factors of
    [] -> [n]
    _  -> factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

-- Taken from
-- http://stackoverflow.com/questions/13517114/count-frequency-of-each-element-in-a-list
-- will write my own implementation later (fingers crossed!)
prime_powers :: Int -> [(Int, Int)]
prime_powers n = map (\x->(head x, length x)) . group . sort $ (prime_factors n)

unique_prime_factors :: Int -> [Int]
unique_prime_factors n = nub $ prime_factors n
