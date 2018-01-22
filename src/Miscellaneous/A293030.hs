module Miscellaneous.A293030  where
import Helpers.Table (n_k)
import Data.Set (Set, empty, insert, notMember)
--   a(n, 1) is the least positive integer not in a previous row.
-- This sequence is a permutation of the natural numbers.
-- Array begins:
-- 1   2   4   5   10  11  13  14  28  29 ...
-- 3   6   7   12  15  16  19  30  33  34 ...
-- 8   9   17  18  20  21  35  36  44  45 ...
-- 22  23  25  26  49  50  52  53  58  59 ...
-- 24  27  51  54  60  63  64  67  73  76 ...
-- 65  66  68  69  74  75  77  78  146 147 ...
-- 70  71  79  80  151 152 160 161 178 179 ...
-- 72  81  153 162 180 189 192 193 196 201 ...
-- 194 195 197 198 203 204 206 207 221 222 ...
-- 208 209 211 212 235 236 238 239 451 452 ...

a293030 :: Int -> Int
a293030 t = a293030_rows !! k !! n where
  (n, k) = n_k (t - 1)

a293030_rows :: [[Int]]
a293030_rows = recurse 1 [1..] where
  recurse n legalTerms = nextRow : recurse (n + 1) newLegalTerms where
    nextRow = arithmeticFreeSequence legalTerms
    newLegalTerms = deleteTermsFrom legalTerms nextRow

-- minuend - subtrahend
-- Works under the assumption that both lists are increasing.
deleteTermsFrom :: [Int] -> [Int] -> [Int]
deleteTermsFrom m [] = m
deleteTermsFrom [] _ = []
deleteTermsFrom minuend@(i:is) subtrahend@(j:js)
  | i < j  = i : deleteTermsFrom is subtrahend
  | i == j = deleteTermsFrom is js
  | i > j  = deleteTermsFrom minuend js

arithmeticFreeSequence :: [Int] -> [Int]
arithmeticFreeSequence validTerms = recurse validTerms empty where
  recurse (v:vs) knownTerms
    | isArithmeticFree v knownTerms = v : recurse vs (insert v knownTerms)
    | otherwise                     = recurse vs knownTerms

isArithmeticFree :: Int -> Set Int -> Bool
isArithmeticFree i knownTerms = all isFree knownTerms where
  isFree j = (2*j - i) `notMember` knownTerms
