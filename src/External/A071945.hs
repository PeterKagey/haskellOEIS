module External.A071945 (a071945) where
import Helpers.ChessSequences (chessMoveCounter, kingN, kingNW, kingSW)
import Helpers.Table (n_k)

a071945 :: Integer -> Integer
a071945 n = case n_k n of (n', k') -> a071945T  (n' + 1) (k' + 1)

-- Longest chain is given by A094727.
a071945T :: Integer -> Integer -> Integer
a071945T = chessMoveCounter visibleCells where
    visibleCells n k = concatMap (\f -> f n k) [kingN, kingNW, kingSW]
