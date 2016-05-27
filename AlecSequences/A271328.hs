module AlecSequences.A271328 (a271328) where
import AlecSequences.A269347 (a269347)
import Data.List (genericIndex)

a271328 :: Integral a => a -> a
a271328 n = genericIndex a271328_list (n - 1)

a271328_list :: Integral a => [a]
a271328_list = map (\n -> a269347 (3 * n) `div` 3) [1..]

-- Some change made this test somewhat slow! (40 ms)
