module Helpers.ListHelpers (concatReplicate, reciprocalSum) where
-- concatReplicate is to replicate as concatMap is to map
concatReplicate :: Int -> [a] -> [a]
concatReplicate n list = take (n * length list) $ cycle list

-- reciprocalSum [2,5] = (1 % 2) + (1 % 5)
--                     = 7 % 10
reciprocalSum :: Integral a => [a] -> Rational
reciprocalSum = sum . map (recip . toRational)
