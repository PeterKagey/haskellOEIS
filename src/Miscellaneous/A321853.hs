module Miscellaneous.A321853 (a321853) where
import Data.List (permutations, span)

type Permutation = [Int]

fillTime :: Integer -> Permutation -> Integer
fillTime c [] = c
fillTime c [_] = c
fillTime c xs = recurse $ span (<x) xs where
  x = maximum xs
  recurse (h, t) = fillTime (c + c') (tail t) where
    c' = fromIntegral $ sum $ map (`subtract` x) h

-- This creates a stack overflow and uses a lot of RAM, I'm not sure why.
a321853 n = sum $ map (fillTime 0) $ permutations [1..n]