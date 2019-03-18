import Data.List (unfoldr)

a306216_list = 0 : 1 : concat (unfoldr nextGeneration [0,1]) where
  nextGeneration l = Just (diff l, l ++ diff l)
  diff xs =  zipWith subtract xs (tail xs)
