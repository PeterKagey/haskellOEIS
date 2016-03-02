module AlecSequences.A269423 (a269423, a269423_list) where

a269423 n = a269423_list !! (n-1)

a269423_list = 1 : remaining 1 where
  remaining k = (sum matching_indices) : remaining (k + 1) where
    matching_indices = filter (\i -> ((k + 1 - (a269423 i)) `mod` i) == 0) [1..k]
