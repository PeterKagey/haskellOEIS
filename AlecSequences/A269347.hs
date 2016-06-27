module AlecSequences.A269347 (a269347) where
import Data.List (genericIndex)

a269347 :: Integer -> Integer
a269347 1 = 1
a269347 n = genericIndex a269347_list (n - 1)

a269347_list :: [Integer]
a269347_list = map a [1..] where
  a n = sum $ filter ((==) 0 . mod n . a269347) [1..n-1]
