import Data.List (findIndices)

type Permutation = [Int]
type Word = [Int]

inv :: Permutation -> Int
inv [] = 0
inv (a:as) = inversion as + length (filter (<a) as)

-- super w = recurse w [] where
--   recurse permutation word
--     | inv permutation == 0 = word
--     | otherwise            = recurse newPermutation newWord where
--       newPermutation =  ++ permutation
--       newWord = word ++ [i..j-2] where
--         i =
--         j =
--
-- ij w = (i, j) where
--   i = last $ findIndices  (\(a,b) -> a > b) $ w `zip` (tail w)
--   j = [i+1..]
