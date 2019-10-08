module Graham.A328045 (a328045, a328045_list) where
import Data.List (genericIndex)
import Graham.A006255 (a006255_list)
import Graham.A300518 (a300518)
import Helpers.SquareHelper (isSquare)
import Helpers.Subsets (allSubsets'')
import Math.NumberTheory.Powers.Fourth (isFourthPower')

a328045 :: Integer -> Integer
a328045 n
  | isSquare n = n
  | agreesWithGraham n   = a300518 n + n
  | otherwise            = last $ head $ filter anyProductIsFourthPower candidateSequences where
    candidateSequences = map (n:) $ allSubsets'' $ possibleBases n

a328045_list :: [Integer]
a328045_list = map a328045 [0..]

raiseUp :: [Integer] -> [[Integer]]
raiseUp = foldr (\a -> concatMap (\ts -> [a^1 : ts, a^2 : ts, a^3 : ts])) [[]]

anyProductIsFourthPower :: [Integer] -> Bool
anyProductIsFourthPower as = any (isFourthPower' . product) $ raiseUp as

agreesWithGraham :: Integer -> Bool
agreesWithGraham n = lowerBound == upperBound n where
  lowerBound = a300518 n + n

upperBound :: Integer -> Integer
upperBound = genericIndex (0 : a006255_list)

possibleBases n = [i | i <- [n+1..ub], n <= (i - a300518 i) || i + a300518 i <= ub] where
  ub = upperBound n
