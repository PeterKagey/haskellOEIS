module AlecSequences.A271468 (a271468) where
import AlecSequences.A271328 (a271328)

a271468 :: Int -> Integer
a271468 n = a271468_list !! (n - 1)

a271468_list :: [Integer]
a271468_list = map toInteger $ filter f [1..] where
  f i = a271328 i /= toInteger (i * i + 1)
