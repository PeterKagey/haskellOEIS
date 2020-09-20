import Data.Set (fromList, delete, member)
import Data.List (permutations, (\\))
import Helpers.ListHelpers (cartesianProduct)

type PermutationWord = [Int]
type Cycle = [Int]
type CycleStructure = [[Int]]

fromWord p = recurse 1 [] (fromList [1..length p]) where
  recurse x currentCycle unseen
    | null unseen       = [reverse currentCycle]
    | x `member` unseen = recurse (p !! (x-1)) (x:currentCycle) (delete x unseen)
    | otherwise         = reverse currentCycle : recurse (minimum unseen) [] unseen

-- fromCycleStructure :: CycleStructure -> PermutationWord
-- fromCycleStructure c = map findValue [1..n] where
--   n = maximum $ concat c
--   findValue = recurse c where
--     recurse (c':cs') i
--       | i `elem` c' = case dropWhile (/= i) c' of [_]     -> head c'
--                                                   (_:a:_) -> a
--       | otherwise = recurse cs' i

normalize :: Int -> CycleStructure -> PermutationWord
normalize n cycleStructure = map f [1..n] where
  f i = foldr g i cycleStructure where
    g a b = case dropWhile (/= b) a of
      [] -> b
      [_] -> head a
      (_:c:_) -> c

product' :: Int -> CycleStructure -> CycleStructure -> CycleStructure
product' n c1 c2 = fromWord $ normalize n $ c1 ++ c2

-- type Strategy = [CycleStructure]

x = map (\l -> map fromWord $ reverse l ++ tail l) $ cartesianProduct 3 $ tail $ permutations [1..3]
f = map (normalize 3) . scanl (product' 3) [[1],[2],[3]]
-- allCandidates = map (\l -> reverse l ++ tail l) $ cartesianProduct 3 $ tail $ permutations [1..3]

f' = map (normalize 4) . scanl (product' 4) [[1],[2],[3],[4]]
x' = map (\l -> map fromWord $ reverse l ++ tail l) $ cartesianProduct 12 $ tail $ permutations [1..4]
