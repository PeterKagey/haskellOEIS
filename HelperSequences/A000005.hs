module HelperSequences.A000005 (a000005) where
import HelperSequences.A032741 (a032741)

a000005 :: Integer -> Int
a000005 = (+ 1) . a032741
