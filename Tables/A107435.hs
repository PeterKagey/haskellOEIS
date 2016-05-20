module Tables.A107435 (a107435) where
import Helpers.Table (n'_k')

a107435 :: Int -> Int
a107435 i = euclid 0 $ n'_k' (i - 1) where
  euclid c (_, 0) = c
  euclid c (a, b) = euclid (c + 1) (b, a `mod` b)
