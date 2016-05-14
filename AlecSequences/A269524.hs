module AlecSequences.A269524 (a269524) where
import Helpers.AlecHelper (buildAlecSequence)
import HelperSequences.A032741 (a032741)

a269524 :: Int -> Integer
a269524 i = a269524_list !! (i - 1)

a269524_list :: [Integer]
a269524_list = buildAlecSequence matchingIndices reducer [1]

matchingIndices :: [Integer] -> [Int]
matchingIndices list = filter f [1..n] where
  n = length list
  f index = a032741 (n + 1) >= a032741 a_i where
    a_i = fromIntegral $ a269524 index

reducer :: [Integer] -> Integer
reducer = toInteger . length
