module Subsets.A064894 (a064894) where
  import Helpers.Subsets (zeroIndexed)

  a064894 :: Integer -> Integer
  a064894 n = foldr gcd 0 $ zeroIndexed n
