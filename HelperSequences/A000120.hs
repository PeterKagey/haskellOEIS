module HelperSequences.A000120 (a000120) where
import Data.Bits ((.&.), shiftR)

a000120 :: Int -> Int
a000120 n = go n 0 where
  go k accum = if k == 0 then accum else go (shiftR k 1) $ (+) accum $ k .&. 1
