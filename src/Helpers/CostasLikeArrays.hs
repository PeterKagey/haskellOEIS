module Helpers.CostasLikeArrays (distinctDistances) where
import Data.List (elemIndex, nub)
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))

type Permutation = [Int]

eachPair :: [a] -> [(a, a)]
eachPair [] = []
eachPair (h:as) = map (\a -> (h, a)) as ++ eachPair as

distinctDistances :: Permutation -> Int
distinctDistances permutation = length $ nub $ map distanceSquare $ eachPair $ zip [0..] permutation where
  distanceSquare ((x1, y1), (x2, y2)) = (x1 - x2)^2 + (y1 - y2)^2
