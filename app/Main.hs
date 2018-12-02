module Main where

import Solutions.Day02 (day02a)

main :: IO ()
main = do
    input <- lines <$> readFile "./input/input02.txt"
    print $ day02a input
