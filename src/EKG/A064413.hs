module EKG.A064413 (a064413, a064413_list) where
import Data.List (genericIndex)
import Helpers.EKGBuilder (buildEKG)

a064413 :: Integral a => a -> a
a064413 n = genericIndex a064413_list (n - 1)

a064413_list :: Integral a => [a]
a064413_list = buildEKG [1, 2]
