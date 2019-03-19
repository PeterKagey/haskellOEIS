module Walk.A324605 (a324605) where
import Data.List (genericLength)
import Helpers.Table (n'_k')
import Helpers.NorthEastWalks (maximalTorusWalks)

a324605 :: Int -> Integer
a324605 n = case n'_k' (n - 1) of (a, b) -> genericLength $ maximalTorusWalks (a + 1)  (b + 1)
