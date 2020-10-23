import Helpers.ListHelpers (cartesianProduct)
import Helpers.Subsets (choose)
import Data.List (genericTake, nub)
import Math.NumberTheory.Powers.Squares (exactSquareRoot)
import Data.Set (Set)
import qualified Data.Set as Set

magnitudeSquared :: [Integer] -> Integer
magnitudeSquared = sum . map (^2)

(+.+) = zipWith (+)
(-.-) = zipWith (-)

-- solutions to x^2 + y^2 + z^2 = n.
magSolutions :: Integer -> [[Integer]]
magSolutions = recurse 3 where
  -- recurse :: Int -> Int -> [[Int]]
  recurse 1 n = case exactSquareRoot n of (Just rootN) -> [[rootN]]
                                          Nothing      -> []
  recurse k n = concatMap (\p -> map (p:) $ recurse (k-1) (n-p^2)) validParts where
    validParts = takeWhile ((<=n) . (^2)) [0..]

-- This reverses; probably this can be fixed with a fold.
-- allSigns [0,1,2] = [[-2,-1,0],[2,-1,0],[-2,1,0],[2,1,0]]
-- allSigns :: [Int] -> [[Int]]
allSigns ns = recurse ns [[]] where
  recurse [] known = known
  recurse (0:ks) known = recurse ks $ map (0:) known
  recurse (k:ks) known = recurse ks $ concatMap (\i -> [-k:i, k:i]) known

isValid' v u = magnitudesMatch && angleIsCorrect where
  magnitudesMatch = magnitudeSquared u == magnitudeSquared v
  angleIsCorrect = -2 * dotProduct == magnitudeSquared v
  dotProduct = sum $ zipWith (*) u v

sidePairs :: Integer -> [[[Integer]]]
sidePairs = filter (\[v,u] -> isValid' v u) . choose 2 . concatMap allSigns . magSolutions

makeHexagon = makePolygon hexagon where
  hexagon u v = [[], [u], [v], [u,u,v], [u,v,v], [u,u,v,v]]

makeTriangle = makePolygon triangle where
  triangle u v = [[], [u], [v]]

makeSquare = makePolygon square where
  square u v = [[], [u], [v], [u, v]]

makePolygon shape u v = Set.fromList $ map (-.- maxCoords) coords where
  maxCoords = foldr1 (zipWith min) coords
  coords = map (foldr (+.+) [0,0,0]) $ shape u v

boundingBox hexagon = foldr1 (zipWith max) $ Set.toList hexagon

-- Hexagons with side length k, all nonnegative coordinates, and touching the coordinate planes
normalizedHexagons k = nub $ map (\[v,u] -> makeHexagon v u) $ sidePairs k

-- Bounding boxes for hexagons with side length sqrt(k)
boundingBoxes = map boundingBox . normalizedHexagons

allBoundingBoxes = map boundingBoxes [1..]

-- Count hexagons in n x n x n cube.
a338322 n = sum $ map (product . map (`subtract` n)) $ filter ((<n) . maximum) $ concat $ genericTake upperBound allBoundingBoxes where
  upperBound = (3 * n^2) `div` 4 + 1
