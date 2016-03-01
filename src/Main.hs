import Data.List

a269427 n = a269427_list !! (n - 1)
a269427_list = 1 : remaining 1 where
    remaining k  = next_term : remaining (k + 1) where
        next_term = length (filter f (zip (take k a269427_list) [1..])) where
            f tuple = (1 + k - fst tuple) `mod` snd tuple == 0

printable = map (\tuple -> show(fst tuple) ++ " " ++ show(snd tuple))(zip [1..20] a269427_list)

main = putStr(intercalate "\n" printable)


--                       X
-- known_sequence = 1 : [?, ?, ?, ?, ?]                         => [1, ?, ?, ?, ?, ?, ?, ...]
--  unknown_sequence[0] = next_term : unknown_sequence[1]       => [1, 1, ?, ?, ?, ?, ?, ...]
--  unknown_sequence[1] = next_term : unknown_sequence[2]       => [1, 1, 3, ?, ?, ?, ?, ...]
--  unknown_sequence[2] = next_term : unknown_sequence[3]       => [1, 1, 3, 3, ?, ?, ?, ...]
--                                                         where
-- next_term = sum_indices (take i known_sequence)         where
-- sum_indices finite_sequence = ...