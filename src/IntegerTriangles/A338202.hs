module IntegerTriangles.A338202 (a338202, a338202_list) where
import Data.Ratio ((%), denominator, Ratio)
type Q = Ratio Integer

-- Note: this is slow because it recomputes each term.
-- A smarter implementation would compute A338201 and let this sequence be
-- the cumulative sum.
a338202 :: Integer -> Int
a338202 n = length $ filter (\[a,b,c] -> a + b > c) $ decreasingRationalSums n 3

a338202_list :: [Int]
a338202_list = map a338202 [1..]

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

allReducedFractions :: Integral a => a -> [Ratio a]
allReducedFractions d = map (% d) $ filter (\n -> n `gcd` d == 1) [1..d-1]
