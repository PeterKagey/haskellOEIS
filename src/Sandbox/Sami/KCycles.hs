import Helpers.Factorials (binomial, factorial)
import Data.Ratio ((%))
-- Probably would be smart to cache this.
c k n m
    | m < 0     = 0
    | m == 0    = f k n
    | otherwise = factorial (k - 1) * (n `binomial` k) * c k (n-k) (m-1) `div` m

f k n = sum $ map (\i -> factorial n * (-1)^i `div` k^i `div` factorial i) [0..n `div` k]

e k n m = half * (one - (numer % denom)) + one where
    one = 1 % 1
    numer = c k (n-1) m
    denom = c k n m
    half = n % 2
