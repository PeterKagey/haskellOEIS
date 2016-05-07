module EKG.A169845 (a169845) where
import Helpers.EKGBuilder (buildEKG)

a169845 :: Int -> Integer
a169845 n = a169845List !! (n - 1)

a169845List :: [Integer]
a169845List = buildEKG [7]
