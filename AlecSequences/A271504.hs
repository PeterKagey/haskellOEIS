module AlecSequences.A271504 (a271504) where
import Helpers.AlecHelper (buildAlecSequence)
import HelperSequences.A032741 (a032741)

a271504 :: Int -> Integer
a271504 i = a271504_list !! (i - 1)

a271504_list :: [Integer]
a271504_list = buildAlecSequence matchingIndices reducer [1]

matchingIndices :: [Integer] -> [Int]
matchingIndices list = filter f [1..n] where
  n = length list
  f index = toInteger (n + 1) `mod` a271504 index == 0

reducer :: [Int] -> Integer
reducer = toInteger . foldr lcm 1
