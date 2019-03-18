module Miscellaneous.Stanley1_21 where

type Composition = [Int]

compositions :: Int -> [Composition]
compositions 0 = [[]]
compositions 1 = [[1]]
compositions n = concatMap compositionsStartingFrom [1..n] where
  compositionsStartingFrom i = map (i:) $ compositions (n - i)

subcompositionCount :: Composition -> Int
subcompositionCount composition = product $ map (length . compositions) composition
