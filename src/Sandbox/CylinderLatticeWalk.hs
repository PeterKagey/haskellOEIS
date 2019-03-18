import Data.Maybe
import Helpers.Table (n_k, n'_k')
import Data.Set (Set, empty, insert, singleton)
-- See TorusLatticeWalk.hs, this is mostly copied and pasted from there with
-- some changes to nextStatesRight and nextStatesUp

-- Notes: I can prove that the number of all cylinder walks is w^h where w is
-- the width, and h is the right. (At each height, take 0, 1, ..., w-1) steps,
-- at the final height, there's only one choice to make.

-- I believe that the maximum number of steps is w*h + (h % w).
-- The main diagonal and off-diagonals seem to have nice structure when counting
-- maximal walks.

data CurrentState = Intersected | Completed (Set Position) | Ongoing State deriving (Show, Eq)
type Position = (Int, Int)
type State = (Position, Set Position)

maximalCylinderWalks n m = recurse [] [Ongoing ((0, 0), singleton (0,0))] where
  recurse completedWalks [] = completedWalks
  recurse completedWalks ongoingStates = recurse completedWalks' ongoingStates' where
    nextStates = concatMap (\s -> [nextStatesRight n m s, nextStatesUp m s]) ongoingStates
    ongoingStates' = filter isOngoing nextStates
    completedWalks' = if null cW then completedWalks else cW where
      cW = filter isCompleted nextStates

allCylinderWalks n m = recurse [] [Ongoing ((0, 0), singleton (0,0))] where
  recurse completedWalks [] = completedWalks
  recurse completedWalks ongoingStates = recurse completedWalks' ongoingStates' where
    nextStates = concatMap (\s -> [nextStatesRight n m s, nextStatesUp m s]) ongoingStates
    ongoingStates' = filter isOngoing nextStates
    completedWalks' = completedWalks ++ filter isCompleted nextStates

nextStatesRight :: Int -> Int -> CurrentState -> CurrentState
nextStatesRight width height (Ongoing ((x, y), pastPositions))
  | newPosition == (0, height) = Completed pastPositions
  | newPosition `elem` pastPositions = Intersected
  | otherwise = Ongoing (newPosition, insert newPosition pastPositions) where
    newPosition = ((x + 1) `mod` width, y)

nextStatesUp :: Int -> CurrentState -> CurrentState
nextStatesUp height (Ongoing ((x, y), pastPositions))
  | newPosition == (0, height)       = Completed pastPositions
  | y == height                      = Intersected
  | newPosition `elem` pastPositions = Intersected
  | otherwise = Ongoing (newPosition, insert newPosition pastPositions) where
    newPosition = (x, y + 1)

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
