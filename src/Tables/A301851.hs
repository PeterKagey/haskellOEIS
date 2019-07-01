module Tables.A301851 where
import Helpers.DistinctDistances (distinctDistances)
import Helpers.Table (n_k)

a301851 :: Integer -> Int
a301851 n = case n_k (n - 1) of (i, j) -> distinctDistances (i + 1) (j + 1)
