module Helpers.BraxtonHelper (allSequences, SymmetricRelation(..), ReflexiveRelation(..)) where

data SymmetricRelation = Symmetric | NonSymmetric deriving (Eq)
data ReflexiveRelation = Reflexive | NonReflexive deriving (Eq)

allSequences :: ReflexiveRelation -> SymmetricRelation -> Int -> Int -> [[Int]]
allSequences reflexivity symmetry upperBound maxLength = f [[]] maxLength where
  f knownSequences 0 = knownSequences
  f knownSequences stepCount = f newSequences (stepCount - 1) where
    newSequences = concatMap (childSequences reflexivity symmetry upperBound) knownSequences

childSequences :: ReflexiveRelation -> SymmetricRelation -> Int -> [Int] -> [[Int]]
childSequences reflexivity symmetry upperBound as = map (:as) validTerms where
  validTerms = filter (isValidExtension reflexivity symmetry as) [1..upperBound]

isValidExtension :: ReflexiveRelation -> SymmetricRelation -> [Int] -> Int -> Bool
isValidExtension _ _ [] _ = True
isValidExtension reflexivity symmetry as@(a:t) c = checkSymmetric && checkReflexive where
  checkSymmetric = (symmetry == NonSymmetric) || a /= c
  checkReflexive = (all validNeighbor $ zip as t) where
    validNeighbor pair = pair /= (c, a) && (reflexivity == NonReflexive || pair /= (a, c))
