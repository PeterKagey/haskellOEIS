import Data.List (findIndices)

type Partition = [Int]

partitionsOf :: Int -> [Partition]
partitionsOf n = partitionsWithMaxPart n n

partitionsWithMaxPart :: Int -> Int -> [Partition]
partitionsWithMaxPart 0 _ = [[]]
partitionsWithMaxPart n maxPart = concatMap nextGeneration $ reverse [1..maxPart `min` n] where
  nextGeneration firstTerm = map (firstTerm:) $ partitionsWithMaxPart (n - firstTerm) firstTerm

partitionsInto3Parts :: Int -> [(Int, Int, Int)]
partitionsInto3Parts n = [(a, b, n - a - b) | a <- aRange, b <- bRange a] where
  aRange = [(n + 2) `div` 3..n]
  bRange a = [(n - a + 1) `div` 2..min a (n - a)]

palindromeInBase :: Int -> Int -> Bool
palindromeInBase b n = rep == reverse rep where
  rep = baseB b n

baseB b 0 = []
baseB b n = n `mod` b : baseB b (n `div` b)

baseBPalindromes :: Int -> [Int]
baseBPalindromes b = filter (palindromeInBase b) [1..]

-- A261678: Even numbers that are not the sum of two binary palindromes.
-- a(A261678(n)) = 0.

--------------------------------------------

type Edge = (Int, Int)
type Status = (Partition, Partition, [Edge])

targetPartitions n = partitionsWithMaxPart (2*n) n

decrementAt :: Int -> Partition -> Partition
decrementAt 0 (a:as) = (a-1) : as
decrementAt n (a:as) = a : decrementAt (n-1) as

nextGen :: (Int, Partition, [Edge]) -> [(Int, Partition, [Edge])]
nextGen (n, partition, edges)
  | null as  = []
  | otherwise = map info $ filter (<=n) as where
    (a:as) = findIndices (>0) partition
    info i = (newIndex, decrementAt a (decrementAt i partition), (a,i):edges) where
      newIndex = if partition !! a == 1 then length partition else i

-- Given a partition, this enumerates the number of multigraphs with that degree sequence.
-- Cf. A209816, A328863.
findEdges :: Partition -> [[Edge]]
findEdges partition = recurse (sum partition `div` 2) [(length partition, partition, [])] where
  recurse 0 status = map (\(_,_,e) -> e) status
  recurse n status = recurse (n - 1) $ concatMap nextGen status

-- n\k | 2 | 3 |  4 |   5 |    6 | 7
-- ----+------------------+------+-----
--  1  | 1 | * |  3 |  *  |   15 | *     (A001147)
--  2  | 1 | 1 |  6 |  22 |  130 | 822   (A002137)
--  3  | 1 | * | 10 |   * |  760 | *     (A108243)
--  4  | 1 | 1 | 15 | 158 | 3355 | 93708
--                          A244878

table :: Int -> Int -> Int
table n k = length $ findEdges $ replicate k n
