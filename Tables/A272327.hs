module Tables.A272327 (a272327, a272327T) where
import Helpers.Table (n_k)
import Data.Maybe (fromJust)
import Data.List (find)

a272327 :: Int -> Integer
a272327 m =  a272327T (toInteger n + 1) (toInteger k + 1) where
  (n, k) = n_k (m - 1)

a272327T :: Integer -> Integer -> Integer
a272327T n k = fromJust $ find (\i -> toInteger i^k `mod` n == 0) [n + 1..]
