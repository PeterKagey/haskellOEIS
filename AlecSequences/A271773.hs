module AlecSequences.A271773 (a271773) where
import Helpers.AlecHelper (buildAlecSequence)

a271773 :: Int -> Integer
a271773 i = a271773_list !! (i - 1)

a271773_list :: [Integer]
a271773_list = buildAlecSequence matchingIndices reducer [0]

matchingIndices :: [Integer] -> [Int]
matchingIndices list = filter f [1..n] where
  n = length list
  f index = ((toInteger n + 1) - a_i) `mod` toInteger(index) == 0 where
    a_i = list !! (index - 1)

reducer :: [Integer] -> Integer
reducer seed = foldr max 0 seed
