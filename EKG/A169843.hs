module EKG.A169843 (a169843) where
import Helpers.EKGBuilder (buildEKG)

a169843 :: Int -> Integer
a169843 n = a169843List !! (n - 1)

a169843List :: [Integer]
a169843List = buildEKG [6]
