module Helpers.BooleanMatrix (rref) where
import Data.Matrix (Matrix, mapRow, getRow, nrows, ncols, getElem)
import Data.Vector (Vector, zipWith, (!))
import Data.List (delete, find)
import Data.Maybe (fromJust)

rref :: Matrix Bool -> Matrix Bool
rref = reduceMatrix 1 1 where
  reduceMatrix r c m
    | r > nrows m || c > ncols m = m
    | otherwise                  = reduceMatrix r' c' m' where
      m' = reduceColumn r c m --
      c' = c + 1
      r' = if getElem r c m' then r + 1 else r

-- Replaces row j with row i XOR row j (elementwise):
--
--                  (T F F)   (T F F)
--                  (F T F)   (F T T)
-- Then xorRows 2 3 (F F T) = (F F T)
--
xorRows :: Int -- row index i
        -> Int -- row index j
        -> Matrix Bool -- initial matrix
        -> Matrix Bool
xorRows i j m = setRow newRow j m where
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

relevantRowIndices :: Int -> Matrix Bool -> [Int]
relevantRowIndices c m = filter (\row -> getElem row c m) [1..nrows m]

reduceColumn :: Int -> Int -> Matrix Bool -> Matrix Bool
reduceColumn r c m
  | done      = m
  | swap      = reduceColumn r c $ transposeRows r' r m
  | otherwise = foldr (xorRows r) m (delete r rowIndices) where
    rowIndices = relevantRowIndices c m
    r' = fromJust $ find (>r) rowIndices
    swap = r `notElem` rowIndices
    done = rowIndices == [r] || all (<r) rowIndices
