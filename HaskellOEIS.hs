module Main where

import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= print . play . head

play :: String -> Integer
play _         = 0
