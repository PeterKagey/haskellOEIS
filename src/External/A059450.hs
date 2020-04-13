module External.A059450 (a059450) where
import Helpers.ChessSequences (chessMoveCounter, queenN, queenSW)
import Helpers.Table (n_k)

a059450 :: Integer -> Integer
a059450 n = case n_k n of (n', k') -> a059450T  (n' + 1) (k' + 1)

-- Longest chain is given by A094727.
-- King analog: A009766
a059450T :: Integer -> Integer -> Integer
a059450T = chessMoveCounter visibleCells where
    visibleCells n k = concatMap (\f -> f n k) [queenN, queenSW]
