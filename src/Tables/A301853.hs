module Tables.A301853 where
import Helpers.DistinctDistances (distinctDistances)
import Helpers.Table (n'_k')

a301853 :: Integer -> Int
a301853 n = case n'_k' (n - 1) of (i, j) -> distinctDistances (i + 1) (j + 1)
