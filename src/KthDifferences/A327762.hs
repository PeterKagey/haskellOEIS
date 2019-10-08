module KthDifferences.A327762 (a327762_list, a327762) where
import Data.Set (Set, singleton, insert, member, empty)
import Data.Maybe (Maybe, mapMaybe)

type KthDifferences = ([Integer], Set Integer)

a327762_list :: [Integer]
a327762_list = take 56 $ recurse ([], empty) where
  recurse kthDifferences = n : recurse ds where
    ds@(n:_,_) = nextTerm kthDifferences

a327762 :: Int -> Integer
a327762 n = a327762_list !! (n-1)

updateDifferences :: [Integer] -> Integer -> KthDifferences -> Maybe KthDifferences
updateDifferences xs nextTerm ([], ds)
  | nextTerm `member` ds = Nothing
  | otherwise            = Just (reverse (nextTerm : xs), insert nextTerm ds)
updateDifferences xs nextTerm (r:rs, ds)
  | nextTerm `member` ds = Nothing
  | otherwise            = updateDifferences (nextTerm : xs) (nextTerm - r) (rs, insert nextTerm ds)

nextTerm :: KthDifferences -> KthDifferences
nextTerm ds = head $ mapMaybe (\t -> updateDifferences [] t ds) [1..]
