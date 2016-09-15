module Tables.A051125 (a051125) where
import Helpers.Table (n_k)

a051125 :: Int -> Int
a051125 i = 1 + max n k where
  (n, k) = n_k (i - 1)
