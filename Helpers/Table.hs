module Helpers.Table (n_k) where
import HelperSequences.A025581 (a025581)
import HelperSequences.A002262 (a002262)

n_k :: Int -> (Int, Int)
n_k i = (a025581 i, a002262 i)
