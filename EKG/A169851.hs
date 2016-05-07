module EKG.A169851 (a169851) where
import Helpers.EKGBuilder (buildEKG)

a169851 :: Int -> Integer
a169851 n = a169851List !! (n - 1)

a169851List :: [Integer]
a169851List = buildEKG [10]
