module Scripts.Helpers.EKGBuilder where

import Data.List (find)
import Data.Maybe (fromJust)

-- Works, sort of, but it gets tied up in re-computing old terms.
build_ekg seed = seed ++ remaining_list (length seed) where
  remaining_list k = next_term : remaining_list (k + 1) where
    next_term = fromJust (find ekg_criteria [2..]) where
      ekg_criteria i = not_in_list i && gcd (build_ekg seed !! (k - 1)) i > 1 where
        not_in_list i = not (i `elem` (take k (build_ekg seed)))
