module EKG.A064413 (a064413) where
import Helpers.EKGBuilder (buildEKG)

a064413 :: Int -> Integer
a064413 n = a064413List !! (n - 1)

a064413List :: [Integer]
a064413List = buildEKG [1, 2]
