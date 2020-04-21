module Poset.A334238 (a334238) where
import Poset.Wichita (wichitaRanks)
import Data.Set (size)

a334238_list :: [Integer]
a334238_list = filter (not . isUnimodal . map size . wichitaRanks) [1..]

a334238 :: Int -> Integer
a334238 n = a334238_list !! (n - 1)

isUnimodal :: [Int] -> Bool
isUnimodal l = null $ dropWhile (uncurry (>=)) $ dropWhile (uncurry (<=)) $ zip l (tail l)
