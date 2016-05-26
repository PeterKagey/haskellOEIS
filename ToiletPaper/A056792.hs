module ToiletPaper.A056792 (a056792) where
import Helpers.BaseRepresentation (toBase)
import HelperSequences.A070939 (a070939)

a056792 :: Int -> Int
a056792 n = a070939 n - 1 + sum (toBase 2 n)
