module ToiletPaper.A260112 (a260112) where
import Helpers.BaseRepresentation (toBase)

a260112 :: Int -> Int
a260112 0 = 0
a260112 n = sum k + length k - 1 where
  k = toBase 4 n
