module EKG.A064413 (a064413, a064413_list) where

import Data.List (find)
import Data.Maybe (fromJust)

-- EKG sequence: a(1) = 1; a(2) = 2; for n > 2, a(n) = smallest number not already used which shares a factor with a(n-1).

a064413 n = a064413_list !! (n - 1)

a064413_list = 1 : 2 : remaining_list 2 where
  remaining_list k = next_term : remaining_list (k + 1) where
    next_term = fromJust (find ekg_criteria [3..]) where
      ekg_criteria i = not_in_list i && gcd (a064413 k) i > 1 where
        not_in_list i = not (i `elem` (take k a064413_list))

-- Inelegant, but it works!