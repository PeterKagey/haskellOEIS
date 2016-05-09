module Palindromes.A249642 (a249642) where
import Helpers.PalindromeCounter (countPalindromes)

a249642 :: Int -> Integer
a249642 n = a249642_list !! n

a249642_list :: [Integer]
a249642_list = countPalindromes 9
