module Helpers.ListHelpers (concatReplicate, reciprocalSum, runLengths, zipWithPadding) where
-- concatReplicate is to replicate as concatMap is to map
concatReplicate :: Int -> [a] -> [a]
concatReplicate n list = take (n * length list) $ cycle list

-- reciprocalSum [2,5] = (1 % 2) + (1 % 5)
--                     = 7 % 10
reciprocalSum :: Integral a => [a] -> Rational
reciprocalSum = sum . map (recip . toRational)

runLengths :: Eq a => [a] -> [Int]
runLengths [] = []
runLengths l = h : runLengths (drop h l) where
  h = length $ takeWhile (== head l) l

-- Inspired by:
-- http://stackoverflow.com/questions/22403029/how-to-zip-lists-with-different-length
zipWithPadding :: a -> [a] -> [a] -> [(a, a)]
zipWithPadding pad (a:as) (b:bs) = (a, b) : zipWithPadding pad as bs
zipWithPadding pad as     []      = zip as (repeat pad)
zipWithPadding pad []      bs     = zip (repeat pad) bs
