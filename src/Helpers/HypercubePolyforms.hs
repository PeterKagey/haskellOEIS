module Helpers.HypercubePolyforms (hypercubePolyformCount) where
import Data.Set (Set)
import qualified Data.Set as Set

type Restriction = (Int, Int)
-- A m-facet of an n-hypercube is a choice of n-m pairs (k, b)
-- where k is in [n] and b is in {0,1}
type Facet       = Set Restriction
type Polyform    = Set Facet

-- poly-m-facet with k cells on an n-cube
hypercubePolyformCount :: Int -> Int -> Int -> Int
hypercubePolyformCount n m k = recurse 0 $ allPolyforms n k (Set.fromList $ map (\i -> (i,0)) [1..n-m])where
  recurse c colorings
    | Set.null colorings = c
    | otherwise          = recurse (c + 1) colorings' where
    colorings' = Set.difference colorings children where
      children = generateChildren n $ minimum colorings

-- memoize this
connectedFacets :: Int -> Facet -> Set Facet
connectedFacets n facet = flatMap replaceFacet oneDeleted where
  newRestrictions = concatMap (`possibleRestrictions` facet) [1..n]
  oneDeleted = Set.map (`Set.delete` facet) facet
  replaceFacet f = Set.fromList $ map (`Set.insert` f) newRestrictions

possibleRestrictions :: Int -> Facet -> [Restriction]
possibleRestrictions m facet = recurse $ Set.lookupGE (m, 0) facet where
  recurse Nothing = [(m, 0), (m, 1)]
  recurse (Just r@(n, _))
    | n == m    = []
    | otherwise = [(m, 0), (m, 1)]

-- A set of all k-polyforms on the n-cube containing the seed facet.
-- When k = 0, this should give the empty polyomino.
allPolyforms :: Int -> Int -> Facet -> Set Polyform
allPolyforms n k seedFacet = recurse k (Set.singleton seedFacet) (connectedFacets n seedFacet) where
  recurse 0 polyform _ = Set.singleton Set.empty
  recurse 1 polyform _ = Set.singleton polyform
  recurse c polyform neighbors = flatMap f neighbors where
    f face = case addToPolyform n face (polyform, neighbors) of (p, ns) -> recurse (c - 1) p ns

addToPolyform :: Int -> Facet -> (Polyform, Set Facet) -> (Polyform, Set Facet)
addToPolyform n facet (polyform, neighbors) = (newPolyform, updatedNeighbors) where
  newPolyform      = Set.insert facet polyform
  updatedNeighbors = Set.union neighbors (connectedFacets n facet) Set.\\ newPolyform

-- i < j
rotation :: Int -> Int -> (Int, Int) -> (Int, Int)
rotation i j (k, b)
  | k == i    = (j, b)
  | k == j    = (i, 1 - b)
  | otherwise = (k, b)

flipFirst :: (Int, Int) -> (Int, Int)
flipFirst (k, b)
  | k == 1    = (k, 1 - b)
  | otherwise = (k, b)

rotation' :: Int -> Int -> Polyform -> Polyform
rotation' i j = Set.map $ Set.map (rotation i j)

flipFirst' :: Polyform -> Polyform
flipFirst' = Set.map (Set.map flipFirst)

allRotations :: Int -> [Polyform -> Polyform]
allRotations n = [rotation' i j | i <- [1..n-1], j <- [i+1..n]]

generateChildren :: Int -> Polyform -> Set Polyform
generateChildren n polyform = recurse (allRotations n) flipped where
  flipped = Set.fromList [polyform, flipFirst' polyform]
  recurse [] symmetries = symmetries
  recurse (r:rs) symmetries = recurse rs s' where
    s' = Set.unions $ scanr (\_ b -> Set.map r b) symmetries [1..3]

-- poly-m-facet with k cells
hypercubePolyforms :: Int -> Int -> Int -> [Polyform]
hypercubePolyforms n m k = recurse $ allPolyforms n k (Set.fromList $ map (\i -> (i,0)) [1..n-m])where
  recurse colorings
    | Set.null colorings = []
    | otherwise          = minimum colorings : recurse colorings' where
    colorings' = Set.difference colorings children where
      children = generateChildren n $ minimum colorings

-----------------------------

flatMap :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
flatMap f s = Set.foldr Set.union Set.empty (Set.map f s)
