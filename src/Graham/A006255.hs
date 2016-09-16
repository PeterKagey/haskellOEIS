module Graham.A006255 (a006255, a999999_list) where
import Graham.A248663 (a248663)
import Data.Matrix (Matrix, matrix, toLists)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Bits
import Helpers.BooleanMatrix (rref)

-- A006255 1 = 1  via [1]
-- A006255 2 = 6  via [2, 3, 6]
-- A006255 3 = 8  via [3, 6, 8]
-- A006255 4 = 4  via [4]
-- A006255 5 = 10 via [5, 8, 10]
-- A006255 6 = 12 via [6, 8, 12]

a006255 = last . a999999_row

-- Initial Boolean matrix for A006255
iMatrix :: Integer -> Matrix Bool
iMatrix n = matrix height (length $ iMatrixColumns n) (uncurry $ entry n) where
  height = maximum $ map bitLength $ iMatrixColumns n

iMatrixColumns :: Integer -> [Integer]
iMatrixColumns n = map a248663 [n + 1..upperBound n] ++ [a248663 n]

upperBound :: Integer -> Integer
upperBound n
  | n > 3     = 2 * n
  | otherwise = 2 * n + 2 -- a(2) = 6; a(3) = 8

entry :: Integer -- For iMatrix n
      -> Int     -- Column i
      -> Int     -- Row j
      -> Bool    -- resultant entry
entry n i j = testBit a248663' (i - 1) where
  a248663' = iMatrixColumns n !! (j - 1)

bitLength :: Integral a => a -> Int
bitLength 0 = 0
bitLength n = 1 + bitLength (n `div` 2)

rrefMatrix :: Integer -> [[Bool]]
rrefMatrix = toLists . rref . iMatrix

a999999_row n = (n:) $ map (adjusted . columnIndex) $ relevantRows where
  adjusted = (1 + n +)
  relevantRows = filter last $ rrefMatrix n
  columnIndex = fromIntegral . fromJust . elemIndex True

a999999_list = concatMap a999999_row [1..]
