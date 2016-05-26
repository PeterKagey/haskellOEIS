module Tables.A007318 (a007318, a007318_row, a007318_tabl) where
import Helpers.Table (n'_k')
import Helpers.Factorials (binomial)

a007318 :: Int -> Integer
a007318 = (!!) a007318_tabl

a007318_row :: Integer -> [Integer]
a007318_row n = map (binomial n) [0..n]

a007318_tabl :: [Integer]
a007318_tabl = concatMap a007318_row [0..]
