module EKG.A169839 (a169839) where
import Helpers.EKGBuilder (buildEKG)

a169839 :: Int -> Integer
a169839 n = a169839List !! (n - 1)

a169839List :: [Integer]
a169839List = buildEKG [4]
