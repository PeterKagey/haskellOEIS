module HelperSequences.A025581 (a025581) where
import HelperSequences.A003056 (a003056)
import HelperSequences.A002262 (a002262)

a025581 :: Int -> Integer
a025581 n = toInteger (a003056 n) - (a002262 n)
