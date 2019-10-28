import Data.Matrix (Matrix, matrix, multStrassenMixed, getRow)
import Data.List (iterate)
import Math.NumberTheory.Powers (integerRoot)
import Data.Vector ((!), zipWith)

-- a :: Int -> Matrix Integer
a328422_list n = foldr1 (Data.Vector.zipWith (+)) $ map (getRow 1) $ take n $ iterate (multStrassenMixed x) x where
  x = startingMatrix n

startingMatrix n = Data.Matrix.matrix n n (\(i,j) -> if i `elem` (a309978_list !! j) then 1 else 0)

a309978_list = map a309978 [0..]

a309978 0 = []
a309978 1 = []
a309978 2 = [1]
a309978 n
  | even n = recurse [n `div` 2, n - 1] 2
  | odd n = recurse [n - 1] 2 where
  recurse c k
    | m == 1 = c
    | n == m + m^k = recurse (m:c) (k+1)
    | otherwise    = recurse c (k+1) where
      m = integerRoot k n

-- 2 -> 3

-- 2 -> 3 -> 4
-- 2 -> 4

-- 2 -> 3 -> 4 -> 5
-- 2 -> 4 -> 5

-- import Data.Ratio ((%), numerator)
-- import Data.Maybe (fromJust)
-- import Numeric.Matrix (Matrix, inv, matrix, minus, row, unit)
--
-- startingMatrix :: Int -> Matrix Rational
-- startingMatrix n = matrix (n,n) (\(i,j) -> if i `elem` (a309978_list !! j) then 1 else 0)
--
-- test n = map numerator $ row 1 $ fromJust $ inv $ minus (unit n) (startingMatrix n)
--
-- a309978_list = map a309978 [0..]
