-- Number of decreasing sequences (p_1/q_1) <=  ... <= (p_k/q_k) that sum to 1
-- where q_i <= n
import Data.Ratio ((%), denominator, Ratio)
type Q = Ratio Integer

decreasingRationalSums :: Integer -> Int -> [[Q]]
decreasingRationalSums largestDenominator k = recurse k [[]] where
  recurse 1 listOfQs = concatMap (`finalExtension` largestDenominator) listOfQs
  recurse counter listOfQs = recurse (counter - 1) (concatMap (`extendList` largestDenominator) listOfQs)

extendList :: [Q] -> Integer -> [[Q]]
extendList qs denom = concatMap (extendListD qs) [1..denom]

extendListD :: [Q] -> Integer -> [[Q]]
extendListD [] denom = map (:[]) $ allReducedFractions denom
extendListD qs@(q:_) denom = map (:qs) $ takeWhile (<=(q `min` r)) $ allReducedFractions denom where
  r = 1 - sum qs

finalExtension :: [Q] -> Integer -> [[Q]]
finalExtension [] _ = [[1 % 1]]
finalExtension qs@(q:_) denom = [r : qs |  (denominator r <= denom) && (r > 0) && r <= q] where
  r = 1 - sum qs

allReducedFractions d = map (%d) $ filter (\n -> n `gcd` d == 1) [1..d-1]

-- Number of unit perimeter triangles with rational side lengths having denominators <= n.
-- 0,0,1,1,2,2,4,5,7,8,12,14,19,21,26,30,38,42,52,60,70,76,90,98,113,122,138,152,173,186,210,226,249,265,306,324,357,377,410,442,482,514,558,592,645,675,727,759,813,848,902,950,1015,1053,1140,1210,1277,1326,1406,1470,1555,1611,1727,1791,1907,1980,2082,2162,2258,2357,2471,2573,2693,2774,2903,3001

-- Number of ways to partition 1 into three rational numbers each with denominator <= n.
-- 0,0,1,2,4,6,10,14,20,26,36,44,58,70,90,106,130,148,178,210,248,278,322,354,404,446,500,560,630,682,762,826,916,988,1144,1216,1330,1420,1544,1672,1812,1938,2092,2232,2436,2568,2752,2880,3076,3226,3434,3626,3860,4022,4362,4650,4908,5118,5408,5664,5974,6214,6664,6920,7376,7666,8040,8360,8734,9130,9550,9958,10402

-- A119983 gives row sums
-- A180360: Triangle read by rows
-- 1
-- 1, 1
-- 1, 2, 1
-- 1, 3, 2, 1
-- 1, 5, 4, 2, 1
-- 1, 6, 6, 5, 3, 1
-- 1, 9,10, 8, 5, 2, 1
