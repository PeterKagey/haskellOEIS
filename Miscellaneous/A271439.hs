module Miscellaneous.A271439 (a271439) where
import HelperSequences.A000217 (a000217)
import HelperSequences.A002024 (a002024)

a271439 :: Int -> Integer
a271439 0 = 0
a271439 n = if isTriangular n then 0 else n' - toInteger (a002024 n) + 1 where
  isTriangular n = n' == last (takeWhile (<= n') a000217_list)
  a000217_list = map a000217 [1..]
  n' = toInteger n
