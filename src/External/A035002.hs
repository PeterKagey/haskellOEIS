module External.A035002 (a035002) where
import Helpers.ChessSequences (chessMoveCounter, queenN, queenW)
import Helpers.Table (n_k)

a035002 :: Integer -> Integer
a035002 n = case n_k (n-1) of (n', k') -> a035002T  (n' + 1) (k' + 1)

-- Longest chain is given by A002024
-- King analog: A007318
a035002T :: Integer -> Integer -> Integer
a035002T = chessMoveCounter visibleCells where
    visibleCells n k = concatMap (\f -> f n k) [queenN, queenW]
