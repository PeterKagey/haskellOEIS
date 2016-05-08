module AlecSequences.A271471 (a271471) where
import AlecSequences.A271468 (a271468)
import Data.Maybe (fromJust)
import Data.List (find)

a271471 :: Int -> Integer
a271471 n = a271471_list !! (n - 1)

a271471_list :: [Integer]
a271471_list = 5 : remainingFrom 1 where
  remainingFrom n = nextTerm : remainingFrom (n + 1) where
    nextTerm = fromJust $ find f validTerms where
      -- Part of the reason this is so slow is because this filter naively
      -- starts at a271468(1), a271468(2), ...
      validTerms = filter (\j -> j > a271471 n) $ map a271468 [1..]
      f a_i = all notDivisibleBySmallerTerm $ take n a271471_list where
        notDivisibleBySmallerTerm k = a_i `mod` k /= 0
