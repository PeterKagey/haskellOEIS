module AlecSequences.A271503 (a271503) where
import Helpers.AlecHelper (buildAlecSequence)
import HelperSequences.A032741 (a032741)

a271503 :: Int -> Integer
a271503 i = a271503_list !! (i - 1)

a271503_list :: [Integer]
a271503_list = buildAlecSequence matchingIndices (toInteger . product) [1]

matchingIndices :: [Integer] -> [Int]
matchingIndices list = filter f [1..n] where
  n = length list
  f index = toInteger (n + 1) `mod` a271503 index == 0
