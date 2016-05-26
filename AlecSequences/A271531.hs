module AlecSequences.A271531 (a271531) where
import Helpers.AlecHelper (buildAlecSequence)

a271531 :: Int -> Integer
a271531 i = a271531_list !! (i - 1)

a271531_list :: [Integer]
a271531_list = buildAlecSequence matchingIndices reducer [1]

matchingIndices :: [Integer] -> [Int]
matchingIndices list = filter f [1..n] where
  n = length list
  f index = ((toInteger n + 1) - a_i) `mod` toInteger index == 0 where
    a_i = list !! (index - 1)

reducer :: [Int] -> Integer
reducer = toInteger . foldr lcm 1
