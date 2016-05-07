module EKG.A169847 (a169847) where
import Helpers.EKGBuilder (buildEKG)

a169847 :: Int -> Integer
a169847 n = a169847List !! (n - 1)

a169847List :: [Integer]
a169847List = buildEKG [8]
