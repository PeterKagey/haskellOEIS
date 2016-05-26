module AlecSequences.A273185 (a273185) where
import Helpers.AlecHelper (buildAlecSequence)

a273185 :: Int -> Integer
a273185 n = a273185_list !! n

a273185_list :: [Integer]
a273185_list = buildAlecSequence matchingIndices (toInteger . length) [0]

matchingIndices :: [Integer] -> [Int]
matchingIndices list = filter f [0..n - 1] where
  n = length list
  f index = isSquare2 (toInteger n + a273185 index)

isSquare2 :: Integral a => a -> Bool
isSquare2 m = m == floor (sqrt $ fromIntegral m) ^ 2
