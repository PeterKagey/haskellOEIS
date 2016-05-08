module Palindromes.A248122 (a248122) where
  import Helpers.PalindromeCounter (countPalindromes)

  a248122 :: Int -> Integer
  a248122 n = a248122_list !! n

  a248122_list :: [Integer]
  a248122_list = countPalindromes 3
