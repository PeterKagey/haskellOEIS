module Helpers.GrahamLinearAlgebra (rrefMatrix, iMatrix') where
import Graham.A248663 (a248663)
import Data.Matrix (Matrix, matrix, toLists)
import Data.Bits
import Helpers.BooleanMatrix (rref, rank)

-- Initial Boolean matrix for A006255
iMatrix :: Integer -> Matrix Bool
iMatrix n = iMatrix' iMatrixColumns n

iMatrixColumns :: Integer -> [Integer]
iMatrixColumns n = map a248663 [n + 1..upperBound n] ++ [a248663 n]

upperBound :: Integer -> Integer
upperBound n
  | n > 3     = 2 * n
  | otherwise = 2 * n + 2 -- a(2) = 6; a(3) = 8

entry :: Integer   -- For iMatrix n
      -> [Integer] -- representation of columns
      -> Int       -- Column i
      -> Int       -- Row j
      -> Bool      -- resultant entry
entry n cols i j = testBit a248663' (i - 1) where
  a248663'       = cols !! (j - 1)

bitLength :: Integral a => a -> Int
bitLength 0 = 0
bitLength n = 1 + bitLength (n `div` 2)

rrefMatrix :: Integer -> Matrix Bool
rrefMatrix = rref . iMatrix

----------------------------------

iMatrix' :: (Integer -> [Integer]) -> Integer -> Matrix Bool
iMatrix' f n = matrix height (length $ x) (uncurry $ entry n x) where
  height = maximum $ map bitLength $ x
  x = f n
