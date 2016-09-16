module Helpers.Primes (primeFactors, primePowers, uniquePrimeFactors, isPrime) where
import Data.List (group, nub, sort, find)
import Control.Arrow ((&&&))
import HelperSequences.A000040 (a000040_list)
import HelperSequences.A238689 (a238689_row)
-- Taken from
-- http://stackoverflow.com/questions/21276844/prime-factors-in-haskell
-- will write my own implementation later (fingers crossed!)

isPrime :: Integral a => a -> Bool
isPrime n = n' `elem` takeWhile (<= n') a000040_list where
  n' = fromIntegral n

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = a238689_row n

-- Taken from
-- http://stackoverflow.com/questions/13517114/count-frequency-of-each-element-in-a-list
-- will write my own implementation later (fingers crossed!)
primePowers :: Integer -> [(Integer, Int)]
primePowers = map (head &&& length) . group . sort . primeFactors

uniquePrimeFactors :: Integer -> [Integer]
uniquePrimeFactors = nub . primeFactors
