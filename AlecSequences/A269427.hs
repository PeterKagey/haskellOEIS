module AlecSequences.A269427 (a269427) where
import Helpers.AlecHelper (buildAlecSequence)

a269427 :: Int -> Integer
a269427 i = a269427_list !! (i - 1)

a269427_list :: [Integer]
a269427_list = buildAlecSequence matchingIndices reducer [1]

matchingIndices :: [Integer] -> [Int]
matchingIndices list = filter f [1..n] where
  n = length list
  f index = (a269427 index - toInteger n - 1) `mod` toInteger index == 0

reducer :: [Int] -> Integer
reducer = toInteger . length
