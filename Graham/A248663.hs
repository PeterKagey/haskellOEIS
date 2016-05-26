module Graham.A248663 (a248663) where
import Helpers.Primes (isPrime)
import Data.Bits (xor)
import Data.Maybe (fromJust)
import Data.List (find)
import HelperSequences.A000040 (a000040_list)

a248663 :: Integer -> Integer
a248663 n
  | n <= 1    = 0
  | isPrime n = 2^e
  | otherwise = a248663 a `xor` a248663 b where
    a = fromJust $ find (\d -> n `mod` d == 0) [2..]
    b = n `div` a
    e = fromJust $ find (\i -> a000040_list !! i == n) [0..]
