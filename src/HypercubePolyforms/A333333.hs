module HypercubePolyforms.A333333 (a333333) where
import Helpers.HypercubePolyforms (hypercubePolyformCount)

a333333_row :: Int -> [Int]
a333333_row n = map (a333333T n) [0..n*2^(n-1)]

a333333T :: Int -> Int -> Int
a333333T n = hypercubePolyformCount n 1

a333333_list :: [Int]
a333333_list = concatMap a333333_row [1..]

a333333 :: Int -> Int
a333333 n = a333333_list !! (n - 1)

-- Polysticks on n-cube:
--  n\k┇ 0 ┃ 1 ┃ 2 ┃ 3 ┃ 4 ┃  5 ┃   6 ┃   7 ┃   8 ┃    9 ┃ 10 ┃ 11 ┃ 12 ┃
-- ┎---╂---╂---╂---╂---╂---╂----╂-----╂-----╂-----╂------╂----╂----╂----┨
-- ┃ 1 ┇ 1 ┃ 1 ┃   ┃   ┃   ┃    ┃     ┃     ┃     ┃      ┃    ┃    ┃    ┃
-- ┃ 2 ┇ 1 ┃ 1 ┃ 1 ┃ 1 ┃ 1 ┃    ┃     ┃     ┃     ┃      ┃    ┃    ┃    ┃      
-- ┃ 3 ┇ 1 ┃ 1 ┃ 1 ┃ 3 ┃ 4 ┃  9 ┃  14 ┃  19 ┃  16 ┃    9 ┃  4 ┃  1 ┃  1 ┃
-- ┃ 4 ┇ 1 ┃ 1 ┃ 1 ┃ 3 ┃ 7 ┃ 21 ┃  72 ┃ 269 ┃ 994 ┃ 3615 ┃  ? ┃  ? ┃  ? ┃
-- ┃ 5 ┇ 1 ┃ 1 ┃ 1 ┃ 3 ┃ 7 ┃ 27 ┃ 110 ┃ 601 ┃   ? ┃    ? ┃  ? ┃  ? ┃  ? ┃
-- ┃ 6 ┇ 1 ┃ 1 ┃ 1 ┃ 3 ┃ 7 ┃ 27 ┃ 121 ┃ 728 ┃   ? ┃    ? ┃  ? ┃  ? ┃  ? ┃
-- ┃ 7 ┇ 1 ┃ 1 ┃ 1 ┃ 3 ┃ 7 ┃ 27 ┃ 121 ┃ 751 ┃   ? ┃    ? ┃  ? ┃  ? ┃  ? ┃
