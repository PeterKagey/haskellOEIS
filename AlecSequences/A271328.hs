module AlecSequences.A271328 (a271328) where
import AlecSequences.A269347 (a269347)

a271328 :: Int -> Integer
a271328 n = a269347 (3 * n) `div` 3
