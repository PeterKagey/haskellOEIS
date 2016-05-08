module AlecSequences.A269347 (a269347) where
import Helpers.AlecHelper (buildAlecSequence)

a269347 :: Int -> Integer
a269347 i = a269347_list !! (i - 1)

a269347_list :: [Integer]
a269347_list = buildAlecSequence matchingIndices reducer [1]

matchingIndices :: [Integer] -> [Int]
matchingIndices list = filter f [1..n] where
  n = length list
  f index = (toInteger n + 1) `mod` (list !! (index - 1)) == 0

--     AlecSequences.generate(terms, :+, e = 0, seed) { |a_i, n| n % a_i == 0 }

reducer :: [Integer] -> Integer
reducer seed = (foldr (+) 0 seed)
