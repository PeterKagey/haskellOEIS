module Helpers.EKGBuilder (buildEKG) where

import Data.List (find, (\\))
import Data.Maybe (fromJust)

buildEKG :: [Integer] -> [Integer]
buildEKG seed = a064413_list where
  a064413_list = seed ++ remaining_list (length seed) where
    remaining_list k = next_term : remaining_list (k + 1) where
      next_term = fromJust $ find nonCoprime unrepresentedInts where
        -- unrepresentedInts is the performance bottle-neck.
        unrepresentedInts = [1..] \\ beginningOfList
        beginningOfList = take k a064413_list
        nonCoprime i = gcd lastTerm i /= 1 where
          lastTerm = a064413_list !! (k - 1)
