module Chess.A334016 (a334016) where
import Helpers.ChessSequences (chessMoveCounter, queenN, queenNW, queenSW)
import Helpers.Table (n_k)

a334016 :: Integer -> Integer
a334016 n = case n_k (n - 1) of (n', k') -> a334016T  (n' + 1) (k' + 1)

-- Longest chain is given by A094727.
-- King analog: A071945
a334016T :: Integer -> Integer -> Integer
a334016T = chessMoveCounter visibleCells where
    visibleCells n k = concatMap (\f -> f n k) [queenN, queenNW, queenSW]
