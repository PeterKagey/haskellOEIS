module HelperSequences.A032741 (a032741) where
  import Data.Bits ((.&.), shiftR)

  -- This is a slow proof of concept.
  a032741 :: Int -> Int
  a032741 n = length $ filter (\k -> n `mod` k == 0) [1..n `div` 2]
