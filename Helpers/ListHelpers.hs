module Helpers.ListHelpers (concatReplicate) where

-- concatReplicate is to replicate as concatMap is to map
concatReplicate :: Int -> [a] -> [a]
concatReplicate n list = take (n * length list) $ cycle list
