import Helpers.Factorials (binomial, factorial)
import Data.List (permutations)

allMenage n = filter (isMenage n) $ permutations [1..n]

isMenage n = recurse 1 where
  recurse i [a]    = a /= i && a /= 1
  recurse i (a:as) = a /= i && a /= i + 1 && recurse (i + 1) as

isGeneralizedMenage n = recurse 1 where
  recurse i (a:as)
    | i == 1    = a /= 1 && a /= 2 && a /= n && recurse (i + 1) as
    | i == n    = a /= 1 && a /= n - 1 && a /= n
    | otherwise = abs (a - i) > 1 && recurse (i + 1) as

isGeneralizedMenage' p = isMenage (length p) p

row n = map (\i -> length $ filter ((==i) . head) m) [3..n] where
  m = allMenage n

-- T(n,k) = Sum_{0<=i<=n-1} Sum_{j=max(i-n+k-1, 0)..min(i,k-2)} (-1)^i*(n-i-1)! * binomial(2k-j-4, j) * binomial(2(n-k+1)-i+j, i-j).

a332709T :: Integer -> Integer -> Integer
a332709T n k
  | k < 3     = 0
  | k > n     = 0
  | otherwise = sum $ map f [0..n-1] where
  f i = coefficient * sum (map g [lower..upper]) where
    lower = max 0 (i + k - n - 1)
    upper = min i (k - 2)
    coefficient = factorial (n - i - 1) * (-1)^i
    g j = binomial (2*k-j-4) j * binomial (2*(n-k+1)-i+j) (i-j)

a332710 n = a332709T n $ (n + 3) `div` 2
