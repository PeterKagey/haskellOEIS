import Data.Matrix (Matrix, matrix, multStrassenMixed, getRow)
import Data.List (iterate)
import Math.NumberTheory.Powers (integerRoot)
import Data.Vector ((!), zipWith)

a328422 n = a328422_list !! (n - 2)

a328422_list :: [Integer]
a328422_list = 1 : recurse 3 where
  recurse n = (sum $ map a328422 $ a328446_row n) : recurse (n + 1)

a328423_list :: [Integer]
a328423_list = recurse a328422_list where
  recurse (a : _ : as) = a : recurse as


a328446_row 0 = []
a328446_row 1 = []
a328446_row 2 = [1]
a328446_row n
  | even n = recurse [n `div` 2, n - 1] 2
  | odd n = recurse [n - 1] 2 where
  recurse c k
    | m == 1 = c
    | n == m + m^k = recurse (m:c) (k+1)
    | otherwise    = recurse c (k+1) where
      m = integerRoot k n
