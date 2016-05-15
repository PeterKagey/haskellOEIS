module Coins.A123663 (a123663) where
import Helpers.ListHelpers (partialSums)

a123663 :: Int -> Integer
a123663 n = a123663_list !! (n - 1)

a123663_list :: [Integer]
a123663_list = partialSums $ 0 : remainder 0

remainder :: Int -> [Integer]
remainder n = sides ++ remainder (n + 1) where
  sides = take (2 * n + 2) $ cycle $ replicate n 2 ++ [1]
