module HelperSequences.A006530 (a006530) where
import Helpers.Primes (primeFactors)

a006530 :: Integer -> Integer
a006530 = maximum . primeFactors
