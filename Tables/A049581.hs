module Tables.A049581 (a049581) where
import Helpers.Table (n_k)

a049581 :: Int -> Int
a049581 i = abs (n - k) where
  (n, k) = n_k i
