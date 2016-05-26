module AlecSequences.A270536 (a270536) where
import Helpers.AlecHelper (buildAlecSequence)
import Helpers.Primes (isPrime)
import HelperSequences.A032741 (a032741)

a270536 :: Int -> Integer
a270536 i = a270536_list !! (i - 1)

a270536_list :: [Integer]
a270536_list = buildAlecSequence matchingIndices (toInteger . sum) [0]

matchingIndices :: [Integer] -> [Int]
matchingIndices list = filter f [1..n] where
  n = length list
  f i = not $ isPrime $ toInteger n + 1 + a270536 i
