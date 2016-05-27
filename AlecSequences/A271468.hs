module AlecSequences.A271468 (a271468) where
import AlecSequences.A271328 (a271328)
import Data.List (genericIndex)

a271468 :: Integral a => a -> a
a271468 n = genericIndex a271468_list (n - 1)

a271468_list :: Integral a => [a]
a271468_list = filter (\i -> a271328 i /= (i^2 + 1)) [1..]

-- Some change made this test incredibly slow!
