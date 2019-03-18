module Graham.A305677 (a305677) where
import PowerDivisibility.A072905 (a072905)
import Helpers.GrahamLinearAlgebra (matrixFromColumnLabels)
import Data.Matrix (Matrix)
import Helpers.BooleanMatrix (nullity)
-- import Helpers.Subsets (allSubsets)
-- import HelperSequences.A007913 (a007913)

-- This counts the number of subsets of [n + 1..a072905 n - 1] whose product has
-- the same squarefree part as n (equivalently a072905 n).
-- 1,2,8,1,64,256,2048,4,1,...

-- bruteForce :: Integer -> Int
-- bruteForce n = length $ hasSameSquarefreePart $ map a007913 allSubsetProducts where
--   hasSameSquarefreePart = filter (== a007913 n)
--   allSubsetProducts = map product $ allSubsets [n + 1..a072905 n - 1]
--
-- (!!) This does not work if A305677(n) = 0.
a305677 :: Integer -> Integer
a305677 = (2^) . nullity . initialMatrix

a305677' :: Integer -> Int
a305677' = nullity . initialMatrix

initialMatrix :: Integer -> Matrix Bool
initialMatrix n = matrixFromColumnLabels [n + 1..a072905 n - 1]
