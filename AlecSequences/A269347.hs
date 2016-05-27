module AlecSequences.A269347 (a269347) where
import Helpers.AlecHelper (buildAlecSequence)
import Data.List (genericIndex, genericLength)

a269347 :: Integral a => a -> a
a269347 i = genericIndex a269347_list (i - 1)

a269347_list :: Integral a => [a]
a269347_list = buildAlecSequence matchingIndices sum [1]

matchingIndices :: Integral a => [a] -> [a]
matchingIndices list = filter f [1..n - 1] where
  n = genericLength list + 1
  f index = n `mod` a_i == 0 where
    a_i = genericIndex list (index - 1)
