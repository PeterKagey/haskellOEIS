module Miscellaneous.Problem70 where
import Data.List (permutations, subsequences, nub)


-- search n k = head $ filter (\s -> count s == k) $ combination n [1..n]

a n = maximum $ map (count (comparisons . pairs)) $ combination n [1..n]
a' n = maximum $ map (count (deduplicate . comparisons . pairs)) $ combination n [1..n]

count differences xs = length $ nub $ map differences $ subsequences xs

pairs xs = zip xs (tail xs)

comparisons = map (uncurry compare)

deduplicate :: Eq a => [a] -> [a]
deduplicate [] = []
deduplicate [a] = [a]
deduplicate (a_1 : a_2 : as)
  | a_1 == a_2 = deduplicate (a_2:as)
  | otherwise  = a_1 : deduplicate (a_2:as)

-- https://stackoverflow.com/a/21775467
combination n xs = mapM (const xs) [1..n]
