module AlecSequences.A096216 (a096216) where
import Helpers.AlecHelper (buildAlecSequence)

a096216 :: Int -> Integer
a096216 n = a096216_list !! (n - 1)

a096216_list :: [Integer]
a096216_list = buildAlecSequence matchingIndices reducer [1]

matchingIndices :: [Integer] -> [Int]
matchingIndices list = filter f [1..n] where
  n = length list
  f index = gcd (toInteger n + 1) (list !! (index - 1)) == 1

reducer :: [Int] -> Integer
reducer = toInteger . length
