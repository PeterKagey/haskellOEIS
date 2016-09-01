module Helpers.Primes (primeFactors, primePowers, uniquePrimeFactors, isPrime) where
import Data.List (group, nub, sort, find)
import Control.Arrow ((&&&))
import HelperSequences.A000040 (a000040_list)
-- Taken from
-- http://stackoverflow.com/questions/21276844/prime-factors-in-haskell
-- will write my own implementation later (fingers crossed!)

isPrime :: Integral a => a -> Bool
isPrime n = Just n' == find (>= n') a000040_list where
  n' = fromIntegral n

primeFactors :: Integral a => a -> [a]
primeFactors n =
  case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (n `div` head factors)
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

-- Taken from
-- http://stackoverflow.com/questions/13517114/count-frequency-of-each-element-in-a-list
-- will write my own implementation later (fingers crossed!)
primePowers :: Integer -> [(Integer, Int)]
primePowers = map (head &&& length) . group . sort . primeFactors

uniquePrimeFactors :: Integer -> [Integer]
uniquePrimeFactors = nub . primeFactors
