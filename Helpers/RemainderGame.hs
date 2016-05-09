module Helpers.RemainderGame (shrinkingDivisorInterations) where

shrinkingDivisorInterations :: Int -> Int -> Integer
shrinkingDivisorInterations n k = iterationCount k 0 where
  iterationCount 0 i = i
  iterationCount b i = iterationCount (n `mod` b) (i + 1)

