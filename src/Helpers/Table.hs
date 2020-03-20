module Helpers.Table (n_k, n'_k', indicesByAntidiagonals) where
import HelperSequences.A025581 (a025581)
import HelperSequences.A002262 (a002262)

-- 0 <= k; 0 <= n
-- n_k 0 = (0, 0)
-- [(0,0),(1,0),(0,1),(2,0),(1,1),(0,2),(3,0),(2,1),(1,2),(0,3),(4,0)]
-- Table by antidiagonals
n_k :: Integral a => a -> (a, a)
n_k i = (a025581 i, a002262 i)

-- 0 <= k <= n
-- n_k 0 = (0, 0)
-- [(0,0),(1,0),(1,1),(2,0),(2,1),(2,2),(3,0),(3,1),(3,2),(3,3),(4,0)]
-- Triangle by rows
n'_k' :: Integral a => a -> (a, a)
n'_k' i = (n + k, k) where
  (n, k) = n_k i

-- (0,0),(1,0),(0,1),(2,0),(1,1),(0,2),(3,0),(2,1),(1,2),(0,3)
-- (0,0),(0,1),(0,2),(0,3)
-- (1,0),(1,1),(1,2),
-- (2,0),(2,1),
-- (3,0),
indicesByAntidiagonals :: Integral a => [(a, a)]
indicesByAntidiagonals = recurse (0,0) where
  recurse i@(0, a) = i : recurse (a + 1, 0)
  recurse i@(a, b) = i : recurse (a - 1, b + 1)
