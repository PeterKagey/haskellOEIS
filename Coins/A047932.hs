module Coins.A047932 (a047932) where
import Helpers.ListHelpers (partialSums)

a047932 :: Int -> Integer
a047932 n = a047932_list !! (n - 1)

a047932_list :: [Integer]
a047932_list = partialSums coinList where
  coinList = [0,1,2,2,2,2,3] ++ remainder 0

remainder :: Int -> [Integer]
remainder n = firstSide ++ middleSides ++ lastSide ++ remainder (n + 1) where
  firstSide = 2 : replicate n 3
  middleSides = take (4 * n + 8) $ cycle $ 2 : replicate (n + 1) 3
  lastSide = 2 : replicate (n + 2) 3
