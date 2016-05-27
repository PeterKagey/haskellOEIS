module AlecSequences.A273185 (a273185) where
import Helpers.AlecHelper (buildAlecSequence)
import Data.List (genericIndex, genericLength)

a273185 :: Integral a => a -> a
a273185 = genericIndex a273185_list

a273185_list :: Integral a => [a]
a273185_list = buildAlecSequence matchingIndices genericLength [0]

matchingIndices :: Integral a => [a] -> [a]
matchingIndices list = filter f [0..n - 1] where
  n = genericLength list
  f index = isSquare (n + a_i) where
    a_i = genericIndex list index

isSquare :: Integral a => a -> Bool
isSquare m = m == floor (sqrt $ fromIntegral m) ^ 2
