module Palindromes.A249639 (a249639) where
  import Helpers.PalindromeCounter (countPalindromes)

  a249639 :: Int -> Integer
  a249639 n = a249639_list !! n

  a249639_list :: [Integer]
  a249639_list = countPalindromes 6
