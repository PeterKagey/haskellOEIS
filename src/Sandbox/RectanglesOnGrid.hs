module Sandbox.RectanglesOnGrid where
import Data.List (permutations, subsequences, nub)
-- A085582 (more generally, A289832)
-- 0, 1, 10, 44, 130, 313, 640, 1192, 2044, 3305, 5078, 7524, 10750, 14993, 20388, 27128, 35448, 45665, 57922, 72636, 89970, 110297, 133976, 161440, 192860, 228857, 269758, 316012, 367974, 426417, 491468, 564120, 644640, 733633, 831674, 939292
a 0 _ = 0
a _ 0 = 0
a n 1 = n * (n + 1) `div` 2
a 1 k = a k 1
a n k = (2 * a n (k - 1)) - a n (k - 2) + maxSize where
  maxSize = sum $ map weighted [1..n] where
    weighted i = (n - i + 1) * f i k

f n k
  | n == k   = n + 2 * length diagonals
  |otherwise = 1 + 2 * length diagonals' where
    diagonals = [a | a <- [1..n], n `mod` a == 0, n `div` a >= 3]
    diagonals' = filter isValid [(a, b) | a <- [1..n], b <- [1..k]] where
      isValid (a, b) = isCongruent && a' >= 2 && a' == b' where
        isCongruent = (n - b) `mod` a == 0 && (k - a) `mod` b == 0
        a' = (n - b) `div` a
        b' = (k - a) `div` b
