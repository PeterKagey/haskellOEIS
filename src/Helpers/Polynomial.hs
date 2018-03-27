module Helpers.Polynomial (validTerms) where
import Data.Ratio (Ratio, (%), denominator, numerator)
import Data.List ((\\), subsequences, genericLength)
import Data.Maybe (mapMaybe)

interpolate :: [(Integer, Integer)] -> Integer -> Ratio Integer
interpolate points x = sum $ map lagrangeProduct points where
  lagrangeProduct (x_j, y_j) = (y_j % 1) * product (map fraction xs) where
    fraction x_k = (x - x_k) % (x_j - x_k)
    xs = filter (/= x_j) $ map fst points

selectIntegers :: [Ratio Integer] -> [Integer]
selectIntegers = mapMaybe asInteger where
  asInteger q
    | denominator q == 1 = Just $ numerator q
    | otherwise          = Nothing

validTerms :: [Integer] -> [Integer]
validTerms ys = [1..] \\ illegalTerms where
  k = genericLength ys
  illegalTerms = selectIntegers $ map (`interpolate` k) allSubsets where
    allSubsets = tail $ subsequences points where
      points = zip [0..] ys
