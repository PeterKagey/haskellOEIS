module Graham.A260510 (a260510) where
import Graham.A006255 (a006255)
import Graham.A248663 (a248663)
import Helpers.GrahamLinearAlgebra (iMatrix')
import Data.Matrix (Matrix)
import Helpers.BooleanMatrix (nullity)

a260510 = (subtract 1) . nullity . iMatrix2

iMatrixColumns2 :: Integer -> [Integer]
iMatrixColumns2 n = map a248663 [n..a006255 n]

iMatrix2 :: Integer -> Matrix Bool
iMatrix2 n = iMatrix' iMatrixColumns2 n
