module Miscellaneous.A279688 (a279688, a279688_list) where
import Helpers.AnagramHelper (possibleAnagramBases, isBaseBAnagram)

a279688 :: Int -> Integer
a279688 n = a279688_list !! (n - 1)

a279688_list :: [Integer]
a279688_list = 0 : filter isAnyAnagram [1..]

isAnyAnagram :: Integer -> Bool
isAnyAnagram n = any (isBaseBAnagram n) $ possibleAnagramBases n
