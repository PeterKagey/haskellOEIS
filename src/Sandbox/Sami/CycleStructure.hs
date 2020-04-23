module Sandbox.CycleStructure where
import Data.Set (fromList, delete, member)
import Data.List (permutations, (\\))

type PermutationWord = [Int]
type Cycle = [Int]
type CycleStructure = [[Int]]

fromWord p = recurse 1 [] (fromList [1..length p]) where
  recurse x currentCycle unseen
    | null unseen       = [reverse currentCycle]
    | x `member` unseen = recurse (p !! (x-1)) (x:currentCycle) (delete x unseen)
    | otherwise         = reverse currentCycle : recurse (minimum unseen) [] unseen

fromCycleStructure c = map findValue [1..n] where
  n = maximum $ concat c
  findValue = recurse c where
    recurse (c':cs') i
      | i `elem` c' = case dropWhile (/= i) c' of [_]     -> head c'
                                                  (_:a:_) -> a
      | otherwise = recurse cs' i

countTwoCycles cycleStructure = length $ filter ((==2) . length) cycleStructure

rescale :: Int -> CycleStructure -> CycleStructure
rescale k = map rescale' where
  rescale' = map (\i -> if i >=k then i + 1 else i)

insertIntoCycle k (a:as) = a:k:as

insertIntoFirstCycle k cs = case rescale k cs of (p':ps') -> insertIntoCycle k p' : ps'

-- phi k cycleStructure@(firstCycle:_)
--   | k == 1                 = [1] : rescale 1 cycleStructure
--   | length firstCycle >= 3 = insertIntoFirstCycle k cycleStructure
--   -- | length firstCycle == 2 = recursiveThing!
--   -- | length firstCycle == 1
--   | otherwise     = error "not yet implemented"

phi :: Int -> CycleStructure -> CycleStructure
phi x cycleStructure = phi' x $ rescale x cycleStructure

phi' :: Int -> CycleStructure -> CycleStructure
phi' x []                    = [[x]]
phi' x [[a]]                 = error "just popped a 2-cycle, don't know what to do!" -- [[x, a]] -- not general enough
phi' x p@([a]:[b]:cs)        = if x < a then [x]:p else [a, x, b]  : cs
phi' x p@([a]:[b1,b2]:cs)    = if x < a then [x]:p else [a, x, b2] : cascade b1 cs
phi' x p@([a]:[b1,b2,b3]:cs) = if x < a then [x]:p else [a, x, b2] : [b1] : phi' b3 cs
phi' x p@([a]:(b1:b2:bs):cs) = if x < a then [x]:p else [a, x, b2] : (b1:bs):cs
phi' x p@([a,b]:cs)          = if x < a then [x]:p else [a, x] : phi' b cs
phi' x p@(p'@(a:_):ps')      = if x < a then [x]:p else insertIntoCycle x p' : ps'

cascade :: Int -> CycleStructure -> CycleStructure
cascade x ([a]:cycles) = [a, x]: cycles
cascade x ([a1, a2]:cycles) = [x, a2] : cascade a1 cycles
cascade x ([a1, a2, a3]:cycles) = [x, a2] : [a1] : [a3] : cycles
cascade x cycles = phi' x cycles -- insertIntoCycle x p' : ps'

x6 = [(a, fromWord b) | a <- [1..7], b <- permutations [1..6]]
x7 = map (uncurry phi) x6

