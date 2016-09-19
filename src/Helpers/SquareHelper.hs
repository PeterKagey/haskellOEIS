module Helpers.SquareHelper (isSquare) where

isSquare :: Integral a => a -> Bool
isSquare m = m == floor (sqrt $ fromIntegral m) ^ 2
