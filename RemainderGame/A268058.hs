module RemainderGame.A268058 (a268058) where
import Helpers.RemainderGame (shrinkingDivisorInterations)

a268058 :: Int -> Integer
a268058 n = maximum $ map (shrinkingDivisorInterations n) [1..n]
