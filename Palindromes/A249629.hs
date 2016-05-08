module Palindromes.A249629 (a249629) where
  import Helpers.PalindromeCounter (countPalindromes)

  a249629 :: Int -> Integer
  a249629 n = a249629_list !! n

  a249629_list :: [Integer]
  a249629_list = countPalindromes 4
