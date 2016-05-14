module Helpers.Primes (primeFactors, primePowers, uniquePrimeFactors) where

import Data.List (group, nub, sort)

-- Taken from
-- http://stackoverflow.com/questions/21276844/prime-factors-in-haskell
-- will write my own implementation later (fingers crossed!)

primeFactors :: Int -> [Int]
primeFactors n =
  case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (n `div` head factors)
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

-- Taken from
-- http://stackoverflow.com/questions/13517114/count-frequency-of-each-element-in-a-list
-- will write my own implementation later (fingers crossed!)
primePowers :: Int -> [(Int, Int)]
primePowers = map (\x->(head x, length x)) . group . sort . primeFactors

uniquePrimeFactors :: Int -> [Int]
uniquePrimeFactors = nub . primeFactors
