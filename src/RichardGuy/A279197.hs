module RichardGuy.A279197 (a279197) where
import Data.List (delete, sort)

a279197 :: Int -> Int
a279197 = length . enumerateA279197

enumerateA279197 :: Int -> [[(Int, Int, Int)]]
enumerateA279197 n = filter (isSelfConjugate n) $ filter isInseparable $ enumerateA104429 n

enumerateA104429 :: Int -> [[(Int, Int, Int)]]
enumerateA104429 n = map fst $ foldr next [([], [1..3*n])] [1..n] where
  next _ = concatMap buildNextGeneration

-- "Inseparable" means that there is no j < n such that the first j of the
-- triples are a partition of 1, ..., 3j.
isInseparable :: [(Int, Int, Int)] -> Bool
isInseparable = isInseparable' . deleteMaximum

isSelfConjugate :: Int -> [(Int, Int, Int)] -> Bool
isSelfConjugate n ts = all hasBuddy ts where
  hasBuddy (a, b, c) = elem c1 ts || elem c2 ts where
    c1 = (m-a, m-b, m-c)
    c2 = (m-b, m-a, m-c)
    m = 3*n + 1

buildNextGeneration :: ([(Int, Int, Int)], [Int]) -> [([(Int, Int, Int)], [Int])]
buildNextGeneration finished @ (_, []) = [finished]
buildNextGeneration (known, remaining) = map (\t -> (t : known, without t)) triples where
  triples = findTriplesWithHead remaining
  without (t_1, t_2, t_3) = delete t_3 $ delete t_2 $ delete t_1 remaining

-- findTriplesWithHead finds all triples (a, b, c) such that:
--  (1) a is the head of the list
--  (2) a + b = 2c
findTriplesWithHead :: [Int] -> [(Int, Int, Int)]
findTriplesWithHead [] = []
findTriplesWithHead (a:as) = map (\c -> (a, 2*c-a, c))$ filter hasSibling as where
  hasSibling c = (2*c-a) `elem` as

isInseparable' :: [(Int, Int, Int)] -> Bool
isInseparable' [] = True
isInseparable' ts = thisRoundIsInseparable && isInseparable' (deleteMaximum ts) where
  thisRoundIsInseparable = any (uncurry (/=)) $ zip flattened [1..]
  flattened = sort $ foldr (\(a, b, c) -> (a:) . (b:) . (c:)) [] ts

deleteMaximum :: [(Int, Int, Int)] -> [(Int, Int, Int)]
deleteMaximum [] = []
deleteMaximum ts = delete max ts where
  max = foldr1 maxTuple ts

maxTuple :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
maxTuple t1@(a, b, c) t2@(a', b', c') = if firstIsBigger then t1 else t2 where
  firstIsBigger = maximum [a, b, c] > maximum [a', b', c']
