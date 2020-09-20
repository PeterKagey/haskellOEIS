

--               ----------------------------------------
--               Haskell programs for Goodstein sequences
--               ----------------------------------------


goodstein, goodstein' :: Integer -> [Integer]
goodstein = g (\x b -> inflate b x - 1)
goodstein' = g (\x b -> inflate b (x - 1))

g :: (Integer -> Integer -> Integer) -> Integer -> [Integer]
g f = (++ [0]) . takeWhile (/= 0) . flip (scanl f) [2..]

inflate :: Integer -> Integer -> Integer
inflate b = sum . map (((b + 1) ^) . (inflate b)) . (h [] 0) where
  h :: [Integer] -> Integer -> Integer -> [Integer]
  h es _ 0 = es
  h es i x = h (replicate (fromInteger d) i ++ es) (i + 1) x'
    where (x', d) = divMod x b

-- ---------------------------------------------------------------------

{- goodstein n and goodstein' n
   are integer sequences starting with n and base b = 2
   calculated as follows:
   - for goodstein: in base b hereditary representation of x
     replace b by b+1 and subtract 1,
   - for goodstein': in base b hereditary representation of x - 1
     replace b by b+1.
   Both versions are equivalent concerning Goodstein's theorem.

   Weisstein's Mathworld and Wikipedia mention the first version, wheras
   the second version is used, e.g., in the book of Schwichtenberg and Wainer.
-}


{- function goodstein is used for:
       A056004, A057650, A059934, A059935, A059936,
       A215409, A056193, A222117, A059933, A211378.
   function goodstein' is used for:
       A222112, A222113.
-}

-- ---------------------------------------------------------------------

-- A056004 Initial step in Goodstein sequences, ...
a056004 = (!! 1) . goodstein

-- A057650 Second step in Goodstein sequences, ...
a057650 = (!! 2) . goodstein

-- A059934 Third step in Goodstein sequences, ...
a059934 = (!! 3) . goodstein

-- A059935 Fourth step in Goodstein sequences, ...
a059935 = (!! 4) . goodstein

-- A059936 Fifth step in Goodstein sequences, ...
a059936 = (!! 5) . goodstein

-- A215409 The Goodstein sequence G(3).
a215409 n = a215409_list !! (n-1)
a215409_list = goodstein 3

-- A056193 Goodstein sequence with a(2)=4: ...
a056193 n = a056193_list !! (n-2)
a056193_list = goodstein 4

-- A222117 Goodstein sequence starting with 15.
a222117 n = a222117_list !! (n-2)
a222117_list = goodstein 15

-- A059933 Goodstein sequence with a(2)=16: ...
a059933 n = a059933_list !! (n-2)
a059933_list = goodstein 16

-- A211378 Goodstein sequence starting with 19.
a211378 n = a211378_list !! (n-2)
a211378_list = goodstein 19

-- -------------------------------------------------------------

-- A222112 Initial step in Goodstein sequences, ...
a222112 = (!! 1) . goodstein'

-- A222113 Goodstein sequence starting with 16.
a222113 n = a222113_list !! (n-1)
a222113_list = goodstein' 16


-- -------------------------------------------------------------
-- Reinhard Zumkeller, Feb 13 2013, reinhard.zumkeller@gmail.com
