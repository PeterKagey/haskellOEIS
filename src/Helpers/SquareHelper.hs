module Helpers.SquareHelper (isSquare) where

isSquare :: Integral a => a -> Bool
isSquare m = m == (integerRoot * integerRoot) where
  integerRoot = floor (sqrt (fromIntegral m)::Double)
