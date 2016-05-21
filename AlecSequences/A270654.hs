module AlecSequences.A270654 (a270654) where
import Helpers.AlecHelper (buildAlecSequence)
import Helpers.Primes (isPrime)
import HelperSequences.A032741 (a032741)

a270654 :: Int -> Integer
a270654 i = a270654_list !! (i - 1)

a270654_list :: [Integer]
a270654_list = buildAlecSequence matchingIndices sum [0]

matchingIndices :: [Integer] -> [Int]
matchingIndices list = filter f [1..n] where
  n = length list
  f i = isPrime $ toInteger n + 1 + a270654 i
