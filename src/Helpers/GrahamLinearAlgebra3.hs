module Helpers.GrahamLinearAlgebra3 (rrefMatrix) where
import Data.List (find)
import Data.Vector (Vector, generate)
import Helpers.F3Vectors (rref)
import HelperSequences.A000040 (a000040_list)
import HelperSequences.A000720 (a000720)
import Helpers.Primes (primePowers)

rrefMatrix :: Integer -> Vector (Vector Int)
rrefMatrix = rref . iMatrix

iMatrix :: Integer -> Vector (Vector Int)
iMatrix n = generate h (\i -> generate l (f n i)) where
  h = fromIntegral $ a000720 $ upperBound n
  l = fromIntegral $ upperBound n - n + 1

-- This can be optimized by memoizing the prime factorization of n.
f :: Integer -> Int -> Int -> Int
f n i j = x where
  x = (`mod` 3) $ ithPrimePower i (n + j') where
    j' = if fromIntegral j == upperBound n - n then 0 else fromIntegral j + 1

upperBound :: Integer -> Integer
upperBound n
  | n == 4    = 9
  | otherwise = 2 * n

-- primePowers uses ~80% of CPU time.
ithPrimePower :: Int -> Integer -> Int
ithPrimePower i = maybe 0 snd . find ithPower . primePowers where
  ithPower (a, _) = a == a000040_list !! i
