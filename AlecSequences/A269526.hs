module AlecSequences.A269526 (a269526) where
import Miscellaneous.A274080 (a274080_row)
import Data.List ((\\))

a269526 :: Integral a => a -> a
a269526 n = head $ [1..] \\ map a269526 (a274080_row n)
