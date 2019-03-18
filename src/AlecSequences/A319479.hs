module AlecSequences.A319479 (a319479) where
import Helpers.Subsets (allSubsets)
import Data.Ratio (Ratio, (%))
import Data.List (nub, genericTake, genericLength)
import Data.Set (Set, empty, insert, member)

type Midpoint = (Ratio Integer, Ratio Integer)
type Point = (Integer, Integer)

average :: [Integer] -> Ratio Integer
average xs = sum xs % genericLength xs

midpoint :: [Point] -> Midpoint
midpoint pts = (average $ map fst pts, average $ map snd pts)

validExtension :: Point -> [Point] -> Bool
validExtension candidate knownSequence = recurse allMidpoints empty where
   allMidpoints = map (midpoint . (candidate:)) $ allSubsets knownSequence
   recurse [] _ = True
   recurse (m:ms) newMidpoints
    | m `member` newMidpoints = False
    | otherwise               = recurse ms (m `insert` newMidpoints)

a319479_points :: [Point]
a319479_points = (1, 1) : remaining 2 where
  remaining :: Int -> [Point]
  remaining n = (fromIntegral n, a_n) : remaining (n + 1) where
    knownSequence = genericTake (n - 1) a319479_points
    a_n = head $ filter (\a_i -> validExtension (fromIntegral n, a_i) knownSequence) [1..]

a319479_list :: [Integer]
a319479_list = map snd a319479_points

a319479 :: Int -> Integer
a319479 n = a319479_list !! (n - 1)
