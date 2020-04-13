module Chess.A279212 (a279212, a279212T) where
import Helpers.ChessSequences (chessMoveCounter, queenN, queenNW, queenW, queenSW)
import Helpers.Table (n_k)

a279212 :: Integer -> Integer
a279212 n = case n_k (n - 1) of (n', k') -> a279212T  (n' + 1) (k' + 1)

-- Longest chain is given by A094727.
-- King analog: A064642
a279212T :: Integer -> Integer -> Integer
a279212T = chessMoveCounter visibleCells where
    visibleCells n k = concatMap (\f -> f n k) [queenN, queenNW, queenW, queenSW]

