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
