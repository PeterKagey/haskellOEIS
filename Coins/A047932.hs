module Coins.A047932 (a047932) where

a047932 :: Int -> Integer
a047932 n = a047932_list !! (n - 1)

a047932_list :: [Integer]
a047932_list = scanl1 (+) $ [0,1,2,2,2] ++ remainder 0

remainder :: Int -> [Integer]
remainder n = sideA ++ sideB ++ remainingSides ++ remainder (n + 1) where
  sideA = 2 : replicate (n + 1) 3
  sideB = 2 : replicate n 3
  remainingSides = take (4 * n + 8) $ cycle $ 2 : replicate (n + 1) 3
