module Helpers.LeafFreeGrids (histograms, validConfigurations) where
import Data.List (subsequences, genericLength)
import qualified Data.Map.Strict as Map
-- Problem 64

data VertexDegree = Isolated | Leaf | Branch deriving (Eq, Show, Ord)
data Edge = Vertical Int Int | Horizontal Int deriving (Show)
type Histogram = Map.Map State Integer
type State = (Int, [VertexDegree])

allStates :: Int -> [State]
allStates n = map (edgeToConnectivity n) $ subsequences $ horizontal ++ vertical where
  horizontal = map Horizontal [1..n]
  vertical = verticalEdges n

connectivityFromCount :: Int -> VertexDegree
connectivityFromCount 0 = Isolated
connectivityFromCount 1 = Leaf
connectivityFromCount _ = Branch

tally :: [Edge] -> [Int]
tally = concatMap countUp where
  countUp (Vertical a b) = [a, b]
  countUp (Horizontal a) = [a]

edgeToConnectivity :: Int -> [Edge] -> State
edgeToConnectivity n edges = (n, map (\i -> connectivityFromCount $ count i theTally) [1..n]) where
  count a as = length $ filter (==a) as
  theTally = tally edges

verticalEdges :: Int -> [Edge]
verticalEdges n = map (\i -> Vertical i (i + 1)) [1..n-1]

-- Expensive
initialHistogram :: Int -> Histogram
initialHistogram n = foldr (`Map.insert` 1) emptyMap (states n) where
  emptyMap = Map.fromList $ map (\s -> (s, 0)) $ allStates n
  states n = map (edgeToConnectivity n) vertical where
    vertical = subsequences $ verticalEdges n

allChildStates :: State -> [State]
allChildStates state@(n, connectivity) = map (edgeToConnectivity n) [v ++ h | v <- vertical, h <- horizontal] where
  vertical = subsequences $ verticalEdges n
  horizontal = map (leafPositions ++) $ subsequences branchPositions where
    f connection = map (Horizontal . fst) $ filter (\(_,c) -> c == connection)$ zip [1..] connectivity
    branchPositions = f Branch
    leafPositions = f Leaf

-- Slow.
childMap :: [State] -> Map.Map State [State]
childMap states = Map.fromList $ map (\s -> (s, allChildStates s)) states

nextGenerationOfState :: State -> [State] -> Map.Map State [State] -> Histogram -> Integer
nextGenerationOfState state@(n, _) states childMap' histogram = sum $ map f stateCount where
  f (state, count) = count * Map.findWithDefault 0 state histogram
  stateCount :: [(State, Integer)]
  stateCount = Map.toList $ Map.map count childMap' where
    count = genericLength . filter (==state)

update :: Map.Map State (Histogram -> Integer) -> Histogram -> Histogram
update stateMap histogram = Map.mapWithKey (\s _ -> (stateMap Map.! s) histogram) histogram

histograms :: Int -> [Histogram]
histograms n = recurse (initialHistogram n) where
  stateMap = nextGenerationOfStateMap n
  recurse h = h : recurse (update stateMap h)

nextGenerationOfStateMap :: Int -> Map.Map State (Histogram -> Integer)
nextGenerationOfStateMap n = Map.fromList $ map (\s -> (s, nextGenerationOfState s states (childMap states))) $ allStates n where
  states = allStates n

validConfigurations :: Histogram -> Integer
validConfigurations = Map.foldrWithKey addValidConfigurations 0 where
  addValidConfigurations state n accum
    | Leaf `elem` snd state = accum
    | otherwise             = n + accum

--------------------------------------------------------------

leafFreeGridsList :: Int -> [Integer]
leafFreeGridsList = map validConfigurations . histograms

a301976_list :: [Integer]
a301976_list = map validConfigurations $ histograms 3

a301976 :: Int -> Integer
a301976 n = a301976_list !! (n - 1)

-- This is slow, because it computes these on n x k grids, but it's fastest
-- to compute this on a short-but-wide grid.
a320101_list :: [Integer]
a320101_list = recurse 1 where
  a n = take n $ map validConfigurations $ histograms n
  recurse k = a k ++ recurse (k + 1)
