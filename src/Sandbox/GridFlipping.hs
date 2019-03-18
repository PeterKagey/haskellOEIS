import Data.List (sort)
-- Used to enumerate https://oeis.org/A019318 in pursuit of
-- Problem 91 (V2).


type Point = (Int, Int)

grid :: Int -> [Point]
grid n = [(a, b) | a <- [0..n-1], b <- [0..n-1]]

subsets :: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

diagonalFlip :: [Point] -> [Point]
diagonalFlip = map (\(a, b) -> (b, a))

turn90 :: Int -> [Point] -> [Point]
turn90 n = map (\(a, b) -> (n-b-1, a))

-- Inefficient
canonicalBoard n = minimum . boards n
boards n board = map sort x ++ map (sort . diagonalFlip) x where
  x = [
    board,
    turn90 n board,
    turn90 n (turn90 n board),
    turn90 n (turn90 n (turn90 n board))]
