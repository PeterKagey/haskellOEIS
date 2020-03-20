listFunction :: Int -> [[Int]]
listFunction d = [[d]]

g n ps = all (\k -> (==0) $ (`mod` n) $ evaluate k ps) [0..n-1]

evaluate x (k:ks) = recurse k ks where
  recurse = foldl (\accum c -> x * accum + c)

list = listFunction 9

f = g 180
falseNegative = filter f list
-- l = [1,0,-1,0,-1,0,1,0,0,0] :: [Int]

-- Monic (1,0,-1)-polynomials of degree n that are always divisible by 4
-- 0,0,0,0,1,5,10,26,91,287,820,2420,7381

-- Monic (1,0,-1)-polynomials of degree n that are always divisible by 5.
-- 0,0,0,0,0,1,3,9,27,54,126,294,686

-- Monic (1,0,-1)-polynomials of degree n that are always divisible by 6.
-- 0,0,0,1,3,6,16,48,138,392,1146,3396,10078

-- 3: [3,6,?
-- 4: [3,4,6,12,?
-- 5: [3,4,5,6,8,10,12,15,24,30,?
-- 6: [3,4,5,6,8,10,12,15,20,24,30,60,?
-- 7: [3,4,5,6,7,8,10,12,14,15,16,20,21,24,30,40,42,48,60,120,?
-- 8: [3,4,5,6,7,8,9,10,12,14,15,16,18,20,21,24,28,30,36,40,42,45,48,60,63,80,84,90,120,126,180,?
-- 9: [3,4,5,6,7,8,9,10,12,14,15,16,18,20,21,24,28,30,35,36,40,42,45,48,56,60,63,70,72,80,84,90,105,120,126,?

-- Numbers d such that there exists a degree d (1,0,-1)-polynomial that is
-- always divisible by n.
--

-- (x^4 - x^2) is always divisble by 12.
-- (x^5 - x) is always divisible by 30.
-- (x^6 -x^2) is always divisble by 60.
-- (x^7 -x^3) is always divisble by 120.
-- (x^8 - x^6 - x^4 + x^2) is always divisible by 180.

-- *Main> length x9
-- 39366
-- *Main> filter (isInKernels 180) $ filter (==[1,0,-1,0,-1,0,1,0,0,0]) x9
-- []
-- *Main> filter (==[1,0,-1,0,-1,0,1,0,0,0]) x9
-- [[1,0,-1,0,-1,0,1,0,0,0]]
-- *Main> filter (isInKernels 180) [[1,0,-1,0,-1,0,1,0,0,0]]
-- [[1,0,-1,0,-1,0,1,0,0,0]]
-- *Main> filter (isInKernels 180) x9
-- []


-----------------------------------
-- *Main> filter f [[1,0,-1,0,-1,0,1,0,0,0]]
-- []
-- *Main> filter (isInKernels 180) [[1,0,-1,0,-1,0,1,0,0,0]]
-- [[1,0,-1,0,-1,0,1,0,0,0]]
