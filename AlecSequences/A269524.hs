module AlecSequences.A269524 (a269524) where
import Helpers.AlecHelper (buildAlecSequence)
import HelperSequences.A032741 (a032741)
import Data.List (genericIndex, genericLength)

a269524 :: Integral a => a -> a
a269524 i = genericIndex a269524_list (i - 1)

a269524_list :: Integral a => [a]
a269524_list = buildAlecSequence matchingIndices genericLength [1]

matchingIndices :: Integral a => [a] -> [a]
matchingIndices list = filter f [1..n] where
  n = genericLength list
  f index = a032741 (n + 1) >= a032741 a_i where
    a_i = genericIndex list (index - 1)
