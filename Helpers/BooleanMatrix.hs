module Helpers.BooleanMatrix (xorRows, transposeRows) where
import Data.Matrix (Matrix, mapRow, getRow)
import Data.Vector (Vector, zipWith, (!))

-- rref :: Matrix Bool -> Matrix Bool
-- rref m =

-- Replaces row i with row i XOR row j (elementwise):
--
--                  (T F F)   (T F F)
--                  (F T F)   (F T T)
-- Then xorRows 2 3 (F F T) = (F F T)
--
xorRows :: Int -- row index i
        -> Int -- row index j
        -> Matrix Bool -- initial matrix
        -> Matrix Bool
xorRows i j m = setRow newRow i m where
  newRow = Data.Vector.zipWith (/=) (getRow i m) (getRow j m)

-- swaps row i with row j.
--                          (F T T)   (F T T)
--                          (F F F)   (T T T)
-- Then transposeRows m 2 3 (T T T) = (F F F)
--
transposeRows :: Int -- row index i
              -> Int -- row index j
              -> Matrix Bool -- initial matrix
              -> Matrix Bool
transposeRows i j m = setRow (row j) i $ setRow (row i) j m where
  row k = getRow k m

setRow :: Vector Bool -> Int -> Matrix Bool -> Matrix Bool
setRow row = mapRow (\k _ -> row ! (k - 1))

-- reduceFirstColumn m = map [1..nrows m]
