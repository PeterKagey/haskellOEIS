module Poset.A334144 (a334144) where
import Poset.Wichita (wichitaRanks)
import Data.Set (Set)
import qualified Data.Set as Set

a334144 :: Integer -> Int
a334144 = maximum . map Set.size . wichitaRanks
