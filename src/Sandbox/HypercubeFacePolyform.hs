import Data.Set (Set, fromList, lookupGE, singleton)
import Data.List (delete)
import qualified Data.Set as Set

-- !! This program has a bug where it considers m-faces that meet at a vertex,
-- even if they don't meet at an m-face!

type Restriction = (Int, Int)
type Facet       = Set Restriction
type Polyform    = Set Facet
-- A m-facet of an n-hypercube is a choice of n-m pairs (k, b)
-- where k is in [n] and b is in {0,1}

-- connectedFacets 4 {(1,0), (3,1)}
--   => [
--        {(1,0), (2,0)},
--        {(1,0), (2,1)},
--        {(1,0), (4,0)},
--        {(1,0), (4,1)},
--        {(2,0), (3,1)},
--        {(2,1), (3,1)},
--        {(2,0), (4,0)},
--        {(2,0), (4,1)},
--        {(2,1), (4,0)},
--        {(2,1), (4,1)},
--        {(2,1), (3,0)},
--        {(2,1), (3,1)}
--      ]

-- memoize this
-- FIXME (!!) Bug! This considers two faces to be connected if they share a
-- vertex, not if they share an (m-1)-facet.
connectedFacets :: Int -> Facet -> Set Facet
connectedFacets n facet = Set.delete facet $ fromList $ map fromList $ recurse (Set.size facet) seeds where
  seeds = map (`possibleRestrictions` facet) [1..n]
  recurse 0 _  = [[]]
  recurse _ [] = []
  recurse c ([r]:rs) = map (r:) (recurse (c-1) rs) ++ recurse c rs
  recurse c ([r1,r2]:rs) = map (r1:) (recurse (c-1) rs) ++ map (r2:) (recurse (c-1) rs) ++ recurse c rs

possibleRestrictions :: Int -> Facet -> [Restriction]
possibleRestrictions m facet = recurse $ Set.lookupGE (m, 0) facet where
  recurse Nothing = [(m, 0), (m, 1)]
  recurse (Just r@(n, _))
    | n == m    = [r]
    | otherwise = [(m, 0), (m, 1)]

-- A set of all k-polyforms on the n-cube containing the seed facet.
-- When k = 0, this should give the empty polyomino.
allPolyforms n k seedFacet = recurse k (Set.singleton seedFacet) (connectedFacets n seedFacet) where
  recurse :: Int -> Polyform -> Set Facet -> Set Polyform
  -- recurse 0 polyform _ = Set.singleton $ Set.empty
  recurse 1 polyform _ = Set.singleton polyform
  recurse c polyform neighbors = flatMap f neighbors where
    f face = case addToPolyform n face (polyform, neighbors) of (p, ns) -> recurse (c - 1) p ns

addToPolyform :: Int -> Facet -> (Polyform, Set Facet) -> (Polyform, Set Facet)
addToPolyform n facet (polyform, neighbors) = (newPolyform, updatedNeighbors) where
  newPolyform      = Set.insert facet polyform
  updatedNeighbors = Set.union neighbors (connectedFacets n facet) Set.\\ newPolyform

------------------------------

-- i < j
rotation i j (k, b)
  | k == i    = (j, b)
  | k == j    = (i, 1 - b)
  | otherwise = (k, b)

flipFirst (k, b)
  | k == 1    = (k, 1 - b)
  | otherwise = (k, b)

rotation' :: Int -> Int -> Polyform -> Polyform
rotation' i j = Set.map $ Set.map (rotation i j)

flipFirst' :: Polyform -> Polyform
flipFirst' = Set.map (Set.map flipFirst)


allRotations n = [rotation' i j | i <- [1..n-1], j <- [i+1..n]]
allRotations' n = [(i, j) | i <- [1..n-1], j <- [i+1..n]]

-- subsetsOfSize 0 _ = [[]]
-- subsetsOfSize _ [] = []
-- subsetsOfSize n (x:xs) =
--     [x : subs | subs <- subsetsOfSize (n-1) xs]
--     ++ subsetsOfSize n xs
--
-- facets n = [(k, b) | k <- [1..n], b <- [0..1]]
--
-- kSubsets 2 n = Set.fromList $ map Set.fromList $ filter (\[(i,_), (j,_)] -> i /= j) $ subsetsOfSize 2 $ facets n
-- kSubsets k n = Set.fromList $ map Set.fromList $ subsetsOfSize k $ facets n

generateChildren n polyform = recurse (allRotations n) flipped where
  flipped = fromList [polyform, flipFirst' polyform]
  recurse [] symmetries = symmetries
  recurse (r:rs) symmetries = recurse rs s' where
    s' = Set.unions $ scanr (\_ b -> Set.map r b) symmetries [1..3]

-- poly-m-facet with k cells
countPolyominos n m k = recurse 0 $ allPolyforms n k (Set.fromList $ map (\i -> (i,0)) [1..n-m])where
  recurse c colorings
    | Set.null colorings = c
    | otherwise          = recurse (c + 1) colorings' where
    colorings' = Set.difference colorings children where
      children = generateChildren n $ minimum colorings

-- poly-m-facet with k cells
countPolyominos' n m k = recurse $ allPolyforms n k (Set.fromList $ map (\i -> (i,0)) [1..n-m])where
  recurse colorings
    | Set.null colorings = []
    | otherwise          = minimum colorings : recurse colorings' where
    colorings' = Set.difference colorings children where
      children = generateChildren n $ minimum colorings

-------------------

exampleFacet1 :: Facet
exampleFacet1 = fromList [(2,1), (3,1)]
exampleFacet2 :: Facet
exampleFacet2 = fromList [(1,1), (2,1)]
exampleFacet3 :: Facet
exampleFacet3 = fromList [(1,0), (3,1)]

examplePolyform :: Polyform
examplePolyform = fromList [exampleFacet1, exampleFacet2]

-----------------------------

-----------------------------

flatMap :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
flatMap f s = Set.foldr Set.union Set.empty (Set.map f s)
