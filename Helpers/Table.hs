module Helpers.Table (n_k, n'_k') where
import HelperSequences.A025581 (a025581)
import HelperSequences.A002262 (a002262)

n_k :: Int -> (Int, Int)
n_k i = (a025581 i, a002262 i)

-- 1 <= k <= n
n'_k' :: Int -> (Int, Int)
n'_k' i = (n + k + 1, k + 1) where
  (n, k) = n_k i
