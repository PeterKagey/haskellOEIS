module Palindromes.A249643 (a249643) where
  import Helpers.PalindromeCounter (countPalindromes)

  a249643 :: Int -> Integer
  a249643 n = a249643_list !! n

  a249643_list :: [Integer]
  a249643_list = countPalindromes 10
