import Data.List (findIndices, permutations)
wreathProductElements k n = [(a, b) | a <- permutations [0..n-1], b <- cartesianProduct n [0..k-1]]

cartesianProduct 0 _        = []
cartesianProduct 1 elements = map (:[]) elements
cartesianProduct n elements = concatMap f $ cartesianProduct (n - 1) elements where
    f es = map (:es) elements

isDerangement (perm, prod) = all (\fp -> (prod !! fp) /= 0) fixedPoints where
    fixedPoints = findIndices (uncurry (==)) $ zip perm [0..]
