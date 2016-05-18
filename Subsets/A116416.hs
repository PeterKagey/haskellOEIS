module Subsets.A116416 (a116416) where
import Helpers.Subsets (oneIndexed)
import Data.Ratio (numerator, (%))

a116416 :: Integer -> Integer
a116416 = numerator . (foldl reciprocalSum 0 . oneIndexed) where
  reciprocalSum accum a = accum + (1 % a)
