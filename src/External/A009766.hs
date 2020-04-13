module External.A009766 (a009766) where
import Helpers.ChessSequences (chessMoveCounter, kingN, kingSW)
import Helpers.Table (n_k)

a009766 :: Integer -> Integer
a009766 n = case n_k n of (n', k') -> a009766T  (n' + 1) (k' + 1)

-- Longest chain is given by A094727.
a009766T :: Integer -> Integer -> Integer
a009766T = chessMoveCounter visibleCells where
    visibleCells n k = concatMap (\f -> f n k) [kingN, kingSW]
