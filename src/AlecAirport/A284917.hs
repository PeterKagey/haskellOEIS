module AlecAirport.A284917 (a284917) where
import Data.List (find, nub)
import Data.Set (Set, notMember, empty, insert, fromList)

a284917 n = map snd a284917_list !! (n - 1)

-- Lexicographically earliest sequence of positive integers such that the Taxicab distance is distict for each pair of distinct points ((n, a(n)), (m, a(m)).
a284917_list = (0,1) : recurse 1 Data.Set.empty where
  recurse :: Int -> Set Int -> [(Int, Int)]
  recurse i knownDistances = (i, nextTerm) : recurse (i + 1) (foldr insert knownDistances (newDistances nextTerm)) where
    newDistances a_i = map (\(j, a_j) -> abs (i - j) + abs (a_i - a_j)) $ take i a284917_list
    nextTerm = head $ filter noDuplicateDistances [1..] where
      noDuplicateDistances a_k = notEquidistantToTwoPoints && distinctDistance where
        newDistances' = newDistances a_k
        distinctDistance = all (`notMember` knownDistances) newDistances'
        notEquidistantToTwoPoints = newDistances' == nub newDistances'
