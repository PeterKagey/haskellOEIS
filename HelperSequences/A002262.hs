module HelperSequences.A002262 (a002262) where
import HelperSequences.A000217 (a000217)
import HelperSequences.A003056 (a003056)

a002262 :: Int -> Integer
a002262 n = toInteger n - (a000217 $ toInteger $ a003056 n)
