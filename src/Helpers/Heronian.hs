module Helpers.Heronian (Triangle, heronianTriangles, isIsosceles, isPrimitive,  primitiveIsoscelesHeronianTriangles) where
import Math.NumberTheory.Powers.Squares (isSquare')
type Triangle = (Integer, Integer, Integer)

increasingSequences :: Integer -> [Triangle]
increasingSequences perimeter = concatMap triangle [1..perimeter `div` 3] where
  triangle shortestSide = map triangle' [shortestSide..(perimeter - shortestSide) `div` 2] where
    triangle' mediumSide = (shortestSide, mediumSide, perimeter - shortestSide - mediumSide)

squareOfArea :: Triangle -> Integer
squareOfArea (a, b, c) = s * (s-a) * (s-b) * (s-c) where
  s = (a + b + c) `div` 2

hasIntegerArea :: Triangle -> Bool
hasIntegerArea = isSquare' . squareOfArea

isValidTriangle :: Triangle -> Bool
isValidTriangle (a, b, c) = a + b > c

isIsosceles :: Triangle -> Bool
isIsosceles (a, b, c) = a == b || b == c

-- Since equilateral triangles are never Heronian.
isScalene :: Triangle -> Bool
isScalene = not . isIsosceles

isPrimitive :: Triangle -> Bool
isPrimitive (a, b, c) = a `gcd` b `gcd` c == 1

heronianTriangles :: Integer -> [Triangle]
heronianTriangles perimeter
  | odd perimeter = []
  | otherwise     = filter hasIntegerArea $ filter isValidTriangle $ increasingSequences perimeter

primitiveIsoscelesHeronianTriangles :: Integer -> [Triangle]
primitiveIsoscelesHeronianTriangles perimeter = filter isPrimitive $ filter isIsosceles $ heronianTriangles perimeter

-- a_list = map (length . isoscelesHeronianTriangles) [1..]
-- b_list = filter (not . null . isoscelesHeronianTriangles) [1..]
-- c_list = filter (>0) a_list

-- d_list = map (length . primitiveIsoscelesHeronianTriangles) [1..]
