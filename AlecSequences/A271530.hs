module AlecSequences.A271530 (a271530) where
import Helpers.AlecHelper (buildAlecSequence)

a271530 :: Int -> Integer
a271530 i = a271530_list !! (i - 1)

a271530_list :: [Integer]
a271530_list = buildAlecSequence matchingIndices product [1]

matchingIndices :: [Integer] -> [Int]
matchingIndices list = filter f [1..n] where
  n = length list
  f index = (toInteger n + 1 - a_i) `mod` toInteger index == 0 where
    a_i = list !! (index - 1)
