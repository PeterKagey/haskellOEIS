module HelperSequences.A032741 (a032741) where
import Data.Bits ((.&.), shiftR)
import Data.List (genericLength)

-- This is a slow proof of concept.
a032741 :: Integral a => a -> a
a032741 n = genericLength $ filter (\k -> n `mod` k == 0) [1..n `div` 2]
