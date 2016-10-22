module Helpers.GrahamLinearAlgebra3 (rrefMatrix) where
import Data.Matrix (Matrix, matrix)
import Data.List (find)
import Helpers.F3Matrix (rref)
import HelperSequences.A000040 (a000040_list)
import HelperSequences.A000720 (a000720)
import Helpers.Primes (primePowers)

rrefMatrix :: Integer -> Matrix Int
rrefMatrix = rref . iMatrix

iMatrix :: Integer -> Matrix Int
iMatrix n = matrix h l $ f n where
  h = fromIntegral $ a000720 $ upperBound n
  l = fromIntegral $ upperBound n - n + 1

-- This can be optimized by memoizing the prime factorization of n.
f :: Integer -> (Int, Int) -> Int
f n (i, j) = x where
  x = (`mod` 3) $ ithPrimePower i (n + j') where
    j' = if fromIntegral j - 1 == upperBound n - n then 0 else fromIntegral j

upperBound :: Integer -> Integer
upperBound n
  | n == 4    = 9
  | otherwise = 2 * n

ithPrimePower :: Int -> Integer -> Int
ithPrimePower i = maybe 0 snd . find ithPower . primePowers where
  ithPower (a, _) = a == a000040_list !! (i - 1)
