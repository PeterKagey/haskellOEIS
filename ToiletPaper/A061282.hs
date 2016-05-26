module ToiletPaper.A061282 (a061282) where
import Helpers.BaseRepresentation (toBase)

a061282 :: Int -> Int
a061282 0 = 0
a061282 n = sum k + length k - 1 where
  k = toBase 3 n
