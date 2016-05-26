module Tables.A268978 (a268978) where
import Helpers.Table (n'_k')
import Tables.A007318 (a007318_row)

a268978 :: Int -> Int
a268978 i = a268978_t n k where
  (n, k) = (toInteger n' + 1, toInteger k' + 1)
  (n', k') = n'_k' (i - 1)

a268978_t :: Integer -> Integer -> Int
a268978_t n k = length $ filter divisibleByK firstNRows where
  firstNRows = concatMap a007318_row [0..n - 1]
  divisibleByK i = i `mod` k == 0
