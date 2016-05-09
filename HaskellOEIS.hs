module Main where
import AlecSequences.A269423
import EKG.A064413
import HelperSequences.A000120
import HelperSequences.A000188
import HelperSequences.A000196
import HelperSequences.A070939

import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= print . play . head

play :: String -> Int
play "a" = a269423 5
play "b" = fromIntegral (a064413 5)
play "c" = a000120 5
play "d" = a000188 5
play "e" = a000196 5
play n = length (n ++ n)
