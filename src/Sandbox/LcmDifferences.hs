import Data.List (nub)
-- Say that the first LCM is the sequence a'(n) = LCM(a(n), a(n+1)) and the
-- k-th LCM is the sequence a''(n) = LCM(a'(n), a'(n+1)) and so on,
-- Then this is the lexicographically earliest sequence where all k-th LCMs
-- contain no duplicate values.

lcmList = recurse [] where
  recurse kthDifferences = n : recurse updatedDifferences where
    n = nextTerm kthDifferences
    updatedDifferences = updateDifferences n kthDifferences

updateDifferences :: Integer -> [[Integer]] -> [[Integer]]
updateDifferences nextTerm [] = [[nextTerm]]
updateDifferences nextTerm (d:ds) = (nextTerm : d) : updateDifferences (lcm x nextTerm) ds where
  x = head d

nextTerm :: [[Integer]] -> Integer
nextTerm ds = head $ filter (isValidConfig ds) [1..]

isValidConfig :: [[Integer]] -> Integer -> Bool
isValidConfig ds n =  all distinctValues $ updateDifferences n ds

distinctValues :: [Integer] -> Bool
distinctValues xs = xs == nub xs
