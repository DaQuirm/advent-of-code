module Main where

import Solutions.Day01 (day01b)

main :: IO ()
main = do
    input <- lines <$> readFile "./input.txt"
    print $ day01b input
