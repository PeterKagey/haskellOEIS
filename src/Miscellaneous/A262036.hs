module Miscellaneous.A262036 (a262036) where
import Data.List (find)
import Data.Maybe (fromJust)
import HelperSequences.A005117 (a005117)
import Miscellaneous.A261865 (a261865)

a262036 :: Integral a => a -> a
a262036 n = fromJust $ find (\k -> a261865 k == a005117 n) [1..]
