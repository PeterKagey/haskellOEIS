import Data.Set (Set, fromList, lookupGE, singleton)
import Data.List (delete)
import qualified Data.Set as Set

-- This counts PSEUDO-polyforms where faces may meet at a vertex, even if they don't meet at an m-face!

type Restriction = (Int, Int)
type Facet       = Set Restriction
type Polyform    = Set Facet

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
allPseduoPolyforms n k seedFacet = recurse k (Set.singleton seedFacet) (connectedFacets n seedFacet) where
  recurse :: Int -> Polyform -> Set Facet -> Set Polyform
  recurse 0 polyform _ = Set.singleton Set.empty
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

generateChildren n polyform = recurse (allRotations n) flipped where
  flipped = fromList [polyform, flipFirst' polyform]
  recurse [] symmetries = symmetries
  recurse (r:rs) symmetries = recurse rs s' where
    s' = Set.unions $ scanr (\_ b -> Set.map r b) symmetries [1..3]

-- poly-m-facet with k cells
countPolyominos n m k = recurse 0 $ allPseduoPolyforms n k (Set.fromList $ map (\i -> (i,0)) [1..n-m])where
  recurse c colorings
    | Set.null colorings = c
    | otherwise          = recurse (c + 1) colorings' where
    colorings' = Set.difference colorings children where
      children = generateChildren n $ minimum colorings

-- poly-m-facet with k cells
countPolyominos' n m k = recurse $ allPseduoPolyforms n k (Set.fromList $ map (\i -> (i,0)) [1..n-m])where
  recurse colorings
    | Set.null colorings = []
    | otherwise          = minimum colorings : recurse colorings' where
    colorings' = Set.difference colorings children where
      children = generateChildren n $ minimum colorings

-------------------

flatMap :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
flatMap f s = Set.foldr Set.union Set.empty (Set.map f s)
