import Data.List (unfoldr)

weaklyDecreasing :: [Int] -> [Int]
weaklyDecreasing [] = []
weaklyDecreasing (n:ns) = recurse 1 n ns where
  recurse c m [] = [c]
  recurse c m (k:ks)
    | k <= m    = recurse (c+1) k ks
    | otherwise = c : recurse 1 k ks

a999999_list = 1 : concat (unfoldr nextGeneration [1]) where
  nextGeneration l = Just (weaklyDecreasing l, l ++ weaklyDecreasing l)
