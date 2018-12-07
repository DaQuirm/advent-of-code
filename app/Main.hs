module Main where

import Solutions.Day03 (day03b)

main :: IO ()
main = do
  input <- lines <$> readFile "./input/input03.txt"
  print $ day03b input
