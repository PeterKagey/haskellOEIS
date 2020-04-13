module External.A008288 (a008288) where
import Helpers.ChessSequences (chessMoveCounter, kingN, kingNW, kingW)
import Helpers.Table (n_k)

a008288 :: Integer -> Integer
a008288 n = case n_k n of (n', k') -> a008288T  (n' + 1) (k' + 1)

-- Longest chain is given by A002024
a008288T :: Integer -> Integer -> Integer
a008288T =  chessMoveCounter visibleCells where
    visibleCells n k = concatMap (\f -> f n k) [kingN, kingW, kingNW]
