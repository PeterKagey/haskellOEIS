module Subsets.A116417 (a116417) where
import Helpers.Subsets (oneIndexed)
import Data.Ratio (denominator, (%))

a116417 :: Integer -> Integer
a116417 = denominator . (foldl reciprocalSum 0 . oneIndexed) where
  reciprocalSum accum a = accum + (1 % a)
