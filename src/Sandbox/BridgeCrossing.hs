module Miscellaneous.BridgeCrossing where
import Data.List (delete)

type Position = ([Int], [Int])
type ElapsedTime = Int
data State = LeftFlash Position ElapsedTime | RightFlash Position ElapsedTime deriving Show

eachPair :: [a] -> [(a, a)]
eachPair [] = []
eachPair (h:as) = map (\a -> (h, a)) as ++ eachPair as

-- A brute force approach.
minimumTime :: [Int] -> Int
minimumTime [] = 0
minimumTime [t] = t
minimumTime speeds = minimumTimeFromState $ LeftFlash (speeds, []) 0

minimumTimeFromState :: State -> Int
minimumTimeFromState (RightFlash ([], _) elapsedTime) = elapsedTime
minimumTimeFromState state = minimum $ map minimumTimeFromState $ children state

currentTime :: State -> Int
currentTime (RightFlash _ time) = time
currentTime (LeftFlash _ time) = time

children :: State -> [State]
children (RightFlash ([], _) _) = error "Nobody should cross!"
children (LeftFlash ([], _) _) = error "Who is holding the flashlight?"
children l@(LeftFlash (left, right) elapsed) = map (uncurry (crossLeft l)) $ eachPair left
children r@(RightFlash (left, right) elapsed) = map (crossRight r) right

crossLeft :: State -> Int -> Int -> State
crossLeft (LeftFlash (left, right) elapsed) personA personB = RightFlash position newElapsed where
  newElapsed = elapsed + (personA `max` personB)
  position = (newLeft, newRight) where
    newLeft = delete personA $ delete personB left
    newRight = personA : personB : right

crossRight :: State -> Int -> State
crossRight (RightFlash (left, right) elapsed) person = LeftFlash position newElapsed where
  newElapsed = elapsed + person
  position = (newLeft, newRight) where
    newLeft = person : left
    newRight = delete person right

a026791_rows :: [[Int]]
a026791_rows = concatMap a026791_n [1..]

a026791_n :: Int -> [[Int]]
a026791_n 0 = [[]]
a026791_n n = concatMap f [1..n] where
  f k = map (k:) $ filter (all (>=k)) $ a026791_n (n - k)
