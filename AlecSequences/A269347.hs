module AlecSequences.A269347 (a269347) where
import Helpers.AlecHelper (buildAlecSequence)

a269347 :: Int -> Integer
a269347 i = a269347_list !! (i - 1)

a269347_list :: [Integer]
a269347_list = buildAlecSequence matchingIndices sum [1]

matchingIndices :: [Integer] -> [Int]
matchingIndices list = filter f [1..n] where
  n = length list
  f index = (toInteger n + 1) `mod` (list !! (index - 1)) == 0
