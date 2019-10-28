-- least k such that A253913(n) - k is a power of k.

isPowerOf :: Integer -> Integer -> Bool
isPowerOf 0 n = n == 0
isPowerOf 1 n = n == 1
isPowerOf _ 1 = True
isPowerOf k n = recurse k where
  recurse m
    | m > n     = False
    | m == n    = True
    | otherwise = recurse (m*k)

a n = filter (\k -> isPowerOf k (n - k)) [2..]
