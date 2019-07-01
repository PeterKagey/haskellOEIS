module Binary.A306922 (a306922, a306922_list) where
import Data.Set (Set, empty, union, singleton, findMin, findMax)
import qualified Data.Set as Set
import Helpers.Binary (splitBits)

a306922 :: Int -> Int
a306922 = (!!) a306922_list

a306922_list :: [Int]
a306922_list = drop 1 $ map powersOfTwo a306922_data

a306922_data :: [Set Int]
a306922_data = singleton 0 : recurse 1 where
  recurse n = nextTerm : recurse (n + 1) where
    nextTerm = foldr (union . combine) empty $ splitBits n where
      combine (a, b) = Set.map (a+) (a306922_data !! b)

powersOfTwo :: Set Int -> Int
powersOfTwo s = length $ filter (`elem` s) numbersToCheck where
  numbersToCheck :: [Int]
  numbersToCheck = takeWhile (<= maxNum) $ dropWhile (< minNum) $ map (2^) [0..] where
    maxNum = findMax s
    minNum = findMin s
