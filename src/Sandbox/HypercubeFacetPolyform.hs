import Data.Set (Set, fromList)
import qualified Data.Set as Set

-- i < j
rotation i j (k, b)
  | k == i    = (j, b)
  | k == j    = (i, 1 - b)
  | otherwise = (k, b)

flipFirst (k, b)
  | k == 1    = (k, 1 - b)
  | otherwise = (k, b)

rotation' i j = Set.map (rotation i j)

allRotations n = [rotation' i j | i <- [1..n-1], j <- [i+1..n]]
allRotations' n = [(i, j) | i <- [1..n-1], j <- [i+1..n]]

subsetsOfSize 0 _ = [[]]
subsetsOfSize _ [] = []
subsetsOfSize n (x:xs) =
    [x : subs | subs <- subsetsOfSize (n-1) xs]
    ++ subsetsOfSize n xs

facets n = [(k, b) | k <- [1..n], b <- [0..1]]

kSubsets 2 n = Set.fromList $ map Set.fromList $ filter (\[(i,_), (j,_)] -> i /= j) $ subsetsOfSize 2 $ facets n
kSubsets k n = Set.fromList $ map Set.fromList $ subsetsOfSize k $ facets n

generateChildren n facetSet = recurse (allRotations n) flipped where
  flipped = fromList [facetSet, Set.map flipFirst facetSet]
  recurse [] symmetries = symmetries
  recurse (r:rs) symmetries = recurse rs s' where
    s' = Set.unions $ scanr (\_ b -> Set.map r b) symmetries [1..3]

countPolyominos k n = recurse 0 $ kSubsets k n where
  recurse c colorings
    | Set.null colorings = c
    | otherwise          = recurse (c + 1) colorings' where
    colorings' = Set.difference colorings children where
      children = generateChildren n $ minimum colorings
