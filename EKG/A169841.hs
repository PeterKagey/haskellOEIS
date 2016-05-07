module EKG.A169841 (a169841) where
import Helpers.EKGBuilder (buildEKG)

a169841 :: Int -> Integer
a169841 n = a169841List !! (n - 1)

a169841List :: [Integer]
a169841List = buildEKG [5]
