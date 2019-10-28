data Tree = Branch [Tree] | Leaf deriving (Eq, Show)

type Forest    = [Tree]
type Partition = [Int]

fromParenthesis :: String -> Tree
-- fromParenthesis "" = Leaf
fromParenthesis "()" = Leaf
fromParenthesis parens = Branch $ map fromParenthesis $ partitionParenthesis $ inner parens

inner :: String -> String
inner "" = error "empty"
inner "a" = error "length 1"
inner s = init $ tail s

-- () is a single leaf
-- (()) is a 2-vertex tree
-- ((())) and (()()) are 3-vertex trees rooted at first and second vertex respectively.
partitionParenthesis :: String -> [String]
partitionParenthesis "" = []
partitionParenthesis parens = recurse 0 "" parens where
  recurse :: Int -> String -> String -> [String]
  recurse 1 s (')':remaining) = reverse (')':s) : partitionParenthesis remaining
  recurse n s ('(':remaining) = recurse (n+1) ('(':s) remaining
  recurse n s (')':remaining) = recurse (n-1) (')':s) remaining
  recurse n s "" = error $ show n
  -- recurse n ("(":remaining)) =

-- gretasFunction :: Forest -> Partition -> Integer
-- gretasFunction f (p:ps) = recurse ps (takeFromTree p f) where
  -- recurse [] forests

size :: Tree -> Int
size Leaf = 1
size (Branch trees) = 1 + sum (map size trees)

takeOne :: Tree -> Forest
takeOne Leaf = []
takeOne (Branch subTrees) = subTrees

takeFromTree :: Int -> Tree -> [Forest]
takeFromTree n t = recurse n [([t], [])] where
  recurse :: Int -> [(Forest, Forest)] -> [Forest]
  recurse 0 f = map (uncurry (++)) f
  recurse n f = recurse (n - 1) (concatMap nextGen f)

takeFromForest :: Int -> Forest -> [Forest]
takeFromForest n forest = concatMap takeTree $ tailsAndInits forest where
  takeTree :: (Forest, Forest) -> [Forest]
  takeTree (t:ts, is)
    | size t >= n = map (++ ts ++ is) $ takeFromTree n t
    | otherwise     = []

nextGen :: (Forest, Forest) -> [(Forest, Forest)]
nextGen (harvestable, oldGrowth) = map (\(t:ts, is) -> (takeOne t ++ ts, is ++ oldGrowth)) $ tailsAndInits harvestable

tailsAndInits :: [a] -> [([a], [a])]
tailsAndInits [] = []
tailsAndInits as = recurse as [] where
  recurse :: [a] -> [a] -> [([a], [a])]
  recurse [] ts = [] -- [([], ts)]
  recurse is'@(i:is) ts = (is', ts) : recurse is (i:ts)
