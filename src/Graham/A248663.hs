module Graham.A248663 (a248663) where
import HelperSequences.A112798 (a112798_row)
import Data.Bits (xor)

a248663 :: Integer -> Integer
a248663 = foldr (xor) 0 . map (\i -> 2^(i - 1)) . a112798_row
