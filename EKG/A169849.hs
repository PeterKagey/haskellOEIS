module EKG.A169849 (a169849) where
import Helpers.EKGBuilder (buildEKG)

a169849 :: Int -> Integer
a169849 n = a169849List !! (n - 1)

a169849List :: [Integer]
a169849List = buildEKG [9]
