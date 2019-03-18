import Data.List (nub)
import Helpers.Subsets (allSubsets)

grid n = [(a, b) | a <- [1..n], b <- [1..n]]

distanceSquared (x_1, y_1) (x_2, y_2) = (x_1 - x_2)^2 + (y_1 - y_2)^2

eachPair :: [a] -> [(a, a)]
eachPair [] = []
eachPair (h:as) = map (\a -> (h, a)) as ++ eachPair as

distinctDistances :: [(Int, Int)] -> Int
distinctDistances points = length $ nub $ map (uncurry distanceSquared) $ eachPair points

minBy :: (a -> Int) -> [a] -> a
minBy f (a:as) = recurse a as where
  recurse knownMin [] = knownMin
  recurse knownMin (x:xs) = if f knownMin <= f x then recurse knownMin xs else recurse x xs

let x = recurse [] (map distinctDistances g5) `zip` (map length g5)
