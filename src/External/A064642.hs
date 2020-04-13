module External.A064642 (a064642) where
import Helpers.ChessSequences (chessMoveCounter, kingN, kingNW, kingW, kingSW)
import Helpers.Table (n_k)

a064642 :: Integer -> Integer
a064642 n = case n_k n of (n', k') -> a064642T  (n' + 1) (k' + 1)

-- Longest chain is given by A094727.
a064642T :: Integer -> Integer -> Integer
a064642T = chessMoveCounter visibleCells where
    visibleCells n k = concatMap (\f -> f n k) [kingN, kingW, kingNW, kingSW]
