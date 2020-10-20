module Miscellaneous.A338271 (a338271) where
import Miscellaneous.A338268 (a338268_row)

a338271 :: Integer -> Integer
a338271 = sum . a338268_row
