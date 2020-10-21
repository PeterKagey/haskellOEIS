module Helpers.DoubleOrIncrement (numberOfGenerations, minimumMatch, numberOfPaths) where
-- The idea is to start with a pair (a, b), and count how many maps of
-- (x, y) |-> (x + 1, 2y) or
-- (x, y) |-> (2x, y + 1)
-- are necessary to make the values equal

-- Naive implementation
numberOfGenerations :: (Integer, Integer) -> Int
numberOfGenerations (a, b) = recurse 0 [(a, b)] where
  recurse counter allPairs
    | any (uncurry (==)) allPairs = counter
    | otherwise                   = recurse (counter + 1) (nextGeneration allPairs) where
      nextGeneration = concatMap (\(a, b) -> [(a + 1, 2*b), (2*a, b + 1)])


-- Naive implementation
minimumMatch :: (Integer, Integer) -> Integer
minimumMatch (a, b) = recurse 0 [(a, b)] where
  recurse counter allPairs
    | any (uncurry (==)) allPairs = minimum $ map fst $ filter (uncurry (==)) allPairs
    | otherwise                   = recurse (counter + 1) (nextGeneration allPairs) where
      nextGeneration = concatMap (\(a, b) -> [(a + 1, 2*b), (2*a, b + 1)])
