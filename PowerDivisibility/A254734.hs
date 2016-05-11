module PowerDivisibility.A254734 (a254734) where
import Data.Maybe (fromJust)
import Data.List (find)

a254734 :: Integer -> Integer
a254734 n = fromJust $ find (\k -> k^4 `mod` n == 0) [n + 1..]
