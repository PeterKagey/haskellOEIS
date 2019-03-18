import Data.Maybe
import Helpers.Table (n_k, n'_k')
import Data.Set (Set, empty, insert)

data CurrentState = Intersected | Completed (Set Position) | Ongoing State deriving (Show, Eq)
type Position = (Int, Int)
type State = (Position, Set Position)

maximalTorusWalks n m = recurse [] [Ongoing ((0, 0), empty)] where
  recurse completedWalks [] = completedWalks
  recurse completedWalks ongoingStates = recurse completedWalks' ongoingStates' where
    nextStates = concatMap (\s -> [nextStatesRight n s, nextStatesUp m s]) ongoingStates
    ongoingStates' = filter isOngoing nextStates
    completedWalks' = if null cW then completedWalks else cW where
      cW = filter isCompleted nextStates

-- 2, 4, 22, 258, 3528, 87830
allTorusWalks n m = recurse [] [Ongoing ((0, 0), empty)] where
  recurse completedWalks [] = completedWalks
  recurse completedWalks ongoingStates = recurse completedWalks' ongoingStates' where
    nextStates = concatMap (\s -> [nextStatesRight n s, nextStatesUp m s]) ongoingStates
    ongoingStates' = filter isOngoing nextStates
    completedWalks' = completedWalks ++ filter isCompleted nextStates

a324604 n = case n'_k' (n - 1) of (a, b) -> length $ allTorusWalks (a + 1)  (b + 1)
-- 2,1,2,1,5,6,1,2,11,8,1,9,14,19,30,1,2,3,2,29,12,1,13,76,27,99,41
a324605 n = case n'_k' (n - 1) of (a, b) -> length $ maximalTorusWalks (a + 1)  (b + 1)
-- 1,2,4,3,5,9,4,8,11,16,5,9,14,19,25,6,12,18,24,29,36,7,13,19,27,33,41
-- 1,
-- 2,4,
-- 3,5,9,
-- 4,8,11,16,
-- 5,9,14,19,25,
-- 6,12,18,24,29,36,
-- 7,13,19,27,33,41,?49?

-- Bad implementation.
a306779 n = case n'_k' (n - 1) of (a, b) -> map stepCount $ maximalTorusWalks (a + 1)  (b + 1)

nextStatesRight :: Int -> CurrentState -> CurrentState
nextStatesRight width (Ongoing ((x, y), pastPositions))
  | newPosition == (0, 0) = Completed pastPositions
  | newPosition `elem` pastPositions = Intersected
  | otherwise = Ongoing (newPosition, insert newPosition pastPositions) where
    newPosition = ((x + 1) `mod` width, y)

nextStatesUp :: Int -> CurrentState -> CurrentState
nextStatesUp height (Ongoing ((x, y), pastPositions))
  | newPosition == (0, 0) = Completed pastPositions
  | newPosition `elem` pastPositions = Intersected
  | otherwise = Ongoing (newPosition, insert newPosition pastPositions) where
    newPosition = (x, (y + 1) `mod` height)

isCompleted :: CurrentState -> Bool
isCompleted (Completed _) = True
isCompleted _             = False

isOngoing :: CurrentState -> Bool
isOngoing (Ongoing _) = True
isOngoing _           = False

-- n x n torus
-- n x m torus
-- n x m torus size of maximal walk
-- n x m torus number of maximal walks
-- * n x n torus maximal
-- n x m torus maximal
-- n x n torus up > right
-- n x m torus up > right

-- n x n cylinder
-- n x m cylinder
-- n x n cylinder maximal
-- n x m cylinder maximal
-- n x n cylinder up > right
-- n x m cylinder up > right

stepCount :: CurrentState -> Int
stepCount Intersected = error "Intersected"
stepCount (Ongoing _) = error "Ongoing!"
stepCount (Completed steps) = length steps
