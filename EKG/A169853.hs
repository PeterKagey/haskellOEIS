module EKG.A169853 (a169853) where
import Helpers.EKGBuilder (buildEKG)

a169853 :: Int -> Integer
a169853 n = a169853List !! (n - 1)

a169853List :: [Integer]
a169853List = buildEKG [11]
