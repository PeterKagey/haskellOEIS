module Miscellaneous.A306698 (a306698, a306698_list) where
import Data.Set (member, singleton, insert)

a306698 :: Int -> Integer
a306698 = (!!) a306698_list

a306698_list :: [Integer]
a306698_list = 0 : recurse 1 0 (singleton 0) where
  recurse n a s = a' : recurse (n + 1) a' (insert a' s) where
    a' = if a'' < 0 || a'' `member` s then a + n else a'' where
      a'' = a - 2*n
