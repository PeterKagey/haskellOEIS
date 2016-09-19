module HelperSequences.A000037 (a000037) where
import Data.List (genericIndex)

a000037 n = genericIndex a000037_list (n - 1)

a000037_list :: [Int]
a000037_list = remove (map (^2) [1..]) [1..] where
  remove ss'@(s:ss) (i:is)
    | i == s = remove ss is
    | otherwise = i : remove ss' is
