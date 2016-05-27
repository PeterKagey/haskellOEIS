module Helpers.EKGBuilder (buildEKG) where

import Data.List (find, (\\))
import Data.Maybe (fromJust)

buildEKG :: Integral a => [a] -> [a]
buildEKG seed = ekg_list where
  ekg_list = seed ++ remaining_list (length seed) where
    remaining_list k = next_term : remaining_list (k + 1) where
      next_term = fromJust $ find nonCoprime unrepresentedInts where
        -- unrepresentedInts is the performance bottle-neck.
        unrepresentedInts = [1..] \\ take k ekg_list
        nonCoprime i = gcd lastTerm i /= 1 where
          lastTerm = ekg_list !! (k - 1)
