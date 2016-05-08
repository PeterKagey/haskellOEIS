module AlecSequences.A270841 (a270841) where

  a270841 :: Integer -> Integer
  a270841 1 = 5
  a270841 n = 2 ^ (n - 2) + n + 1

