-- class OEIS

--   def self.a262343(n)
--     k = n/6
--     case n % 6
--     when 0 then 3  * k - 1 #     n/2 - 1
--     when 1 then 18 * k - 3 # 3 * n   - 6
--     when 2 then 9  * k     # 3 * n/2 - 3
--     when 3 then 6  * k + 1 #     n   - 2
--     when 4 then 9  * k + 3 # 3 * n/2 - 3
--     when 5 then 18 * k + 9 # 3 * n   - 6
--     end

--   end

-- end

module Miscellaneous.A262343 (a262343) where

a262343 :: Integral a => a -> a
a262343 n
  | m == 0 =     div n 2 - 1
  | m == 1 = 3 *     n   - 6
  | m == 2 = 3 * div n 2 - 3
  | m == 3 =         n   - 2
  | m == 4 = 3 * div n 2 - 3
  | m == 5 = 3 *     n   - 6 where
    m = mod n 6
