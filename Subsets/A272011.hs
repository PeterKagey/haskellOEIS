module Subsets.A272011 (a272011) where
import Helpers.Subsets (zeroIndexed)

a272011 n = a272011List !! n

a272011List = list 0 where
  list n = (zeroIndexed n) ++ list (n + 1)
