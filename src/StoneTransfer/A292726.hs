module StoneTransfer.A292726 (a292726, a292726_list) where
import Data.List (delete, nub, sort, null)
import Data.Set (Set, empty, insert, notMember)
-- The best structure for this would probably be a Scala-like Map[Int, Int],
-- which behaves like a histogram.
-- This is much slower than the Ruby implementation.

a292726 n = length $ enumerateFrom $ replicate n 1
a292726_list = map a292726 [1..]

count :: Eq a => a -> [a] -> Int
count a = length . filter (== a)

multiples :: [Int] -> [Int]
multiples as = filter (\a -> count a as > 1) $ nub as

descendants :: Int -> [Int] -> [[Int]]
descendants pileSize currentGen = map (transferStones pileSize currentGen) [1..pileSize]

transferStones :: Int -> [Int] -> Int -> [Int]
transferStones pileSize currentGen numberToTransfer = sort $ filter (>0) $ biggerPile : smallerPile : reducedGroup where
  biggerPile = pileSize + numberToTransfer
  smallerPile = pileSize - numberToTransfer
  reducedGroup = delete pileSize (delete pileSize currentGen)

nextGeneration :: [Int] -> [[Int]]
nextGeneration currentGen = concatMap (`descendants` currentGen) $ multiples currentGen

enumerateFrom :: [Int] -> Set [Int]
enumerateFrom initialGeneration = recurse [initialGeneration] empty where
  recurse :: [[Int]] -> Set [Int] -> Set [Int]
  recurse currentGen ledger = if null newGen then updatedLedger else recurse newGen updatedLedger where
    newGen = filter (`notMember` ledger) $ concatMap nextGeneration currentGen
    updatedLedger = insertList currentGen ledger

insertList list set = foldr insert set list
