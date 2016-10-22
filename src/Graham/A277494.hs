module Graham.A277494 (a277494) where
import Helpers.GrahamLinearAlgebra3 (rrefMatrix)
import Data.Matrix (getCol, ncols, Matrix)
import Data.List (elemIndices, findIndex)
import Data.Vector (toList)
import Data.Maybe (mapMaybe)

a277494 :: Integer -> Int
a277494 n
  | array == [0] = n'
  | otherwise    = last array where
    array = 0 : map fst (a277494' n)
    n' = fromIntegral n

a277494' :: Integer -> [(Int, Int)]
a277494' n = (n', 1) : mapMaybe matchingColumns lastColumnsIndices where
  matchingColumns (p_i, r_i) = fmap (\a -> (n' + 1 + a, 3 - p_i)) matchingColumnsWithValues where
    matchingColumnsWithValues = findIndex matching $ columns m where
    matching column = elemIndices 1 column == [r_i]
  lastColumnsIndices = filter (\(a, _) -> a /= 0) $ zip lastColumn [0..] where
    lastColumn = last $ columns m
  m = rrefMatrix n
  n' = fromIntegral n

columns :: Matrix a -> [[a]]
columns m = map (\c_i -> toList $ getCol c_i m) [1..ncols m]
