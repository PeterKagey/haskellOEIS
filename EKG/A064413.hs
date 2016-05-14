module EKG.A064413 (a064413) where
import Helpers.EKGBuilder (buildEKG)

a064413 :: Int -> Integer
a064413 n = a064413_list !! (n - 1)

a064413_list :: [Integer]
a064413_list = buildEKG [1, 2]
