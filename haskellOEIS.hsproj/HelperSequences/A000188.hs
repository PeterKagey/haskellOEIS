module HelperSequences.A000188 (a000188) where
  import Helpers.Primes (prime_powers)

-- (1) Number of solutions to x^2 = 0 (mod n). (2) Also square root of largest
-- square dividing n. (3) Also Max_{ d divides n } GCD(d, n/d).

  a000188 n = product $ map (\t -> fst t ^ (snd t `div` 2)) $ prime_powers n