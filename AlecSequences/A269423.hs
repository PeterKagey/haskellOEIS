module AlecSequences.A269423 (a269423) where
import Helpers.AlecHelper (buildAlecSequence)

a269423 :: Int -> Integer
a269423 i = a269423_list !! (i - 1)

a269423_list :: [Integer]
a269423_list = buildAlecSequence matchingIndices sum [1]

matchingIndices :: [Integer] -> [Int]
matchingIndices list = filter f [1..n] where
  n = length list
  f index = ((toInteger n + 1) - a_i) `mod` toInteger index == 0 where
    a_i = list !! (index - 1)
