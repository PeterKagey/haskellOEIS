module AlecSequences.A271774 (a271774) where
import Helpers.AlecHelper (buildAlecSequence)

a271774 :: Int -> Integer
a271774 i = a271774_list !! (i - 1)

a271774_list :: [Integer]
a271774_list = buildAlecSequence matchingIndices reducer [1]

matchingIndices :: [Integer] -> [Int]
matchingIndices list = filter f [1..n] where
  n = length list
  f index = toInteger (n + 1) `mod` a271774 index == 0

reducer :: [Integer] -> Integer
reducer seed = foldr max 0 seed
