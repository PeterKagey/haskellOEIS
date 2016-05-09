module Miscellaneous.A137734 (a137734) where

a137734 :: Int -> Integer
a137734 n = a137734List !! n

a137734List :: [Integer]
a137734List = 1 : remaining 1 1 where
  remaining n maxTerm = nextTerm : remaining (n + 1) (max nextTerm maxTerm) where
    nextTerm = ceiling $ toRational n / (toRational maxTerm)
