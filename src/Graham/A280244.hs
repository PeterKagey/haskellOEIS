module Graham.A280244 (a280244) where
import Graham.A006255 (a006255)
import Graham.A248663 (a248663)
import Helpers.Primes (isPrime)
import Helpers.Subsets (allSubsets)
import Data.Bits (xor)
import Data.List (sort)
import Helpers.Primes (uniquePrimeFactors)

a280244 :: Int -> Integer
a280244 n = a280244_list !! (n - 1)

a280244_list :: [Integer]
a280244_list = concat $ concatMap a280244_row [1..]

a280244_row :: Integer -> [[Integer]]
a280244_row n
  | n == a006255 n = [[n]]
  | otherwise = sort $ map sandwich $ filter squareProduct $ candidates n where
    target = xor (a248663 n) (a248663 $ a006255 n)
    squareProduct list = foldr (xor . a248663) 0 list == target
    sandwich l = n : l ++ [a006255 n]

candidates :: Integer -> [[Integer]]
candidates 2 = allSubsets [3..4]
candidates n = allSubsets $ filter (not . isPrime) [n + 1..a006255 n - 1]
