module EKG.A169855 (a169855) where
import Helpers.EKGBuilder (buildEKG)

a169855 :: Int -> Integer
a169855 n = a169855List !! (n - 1)

a169855List :: [Integer]
a169855List = buildEKG [12]
