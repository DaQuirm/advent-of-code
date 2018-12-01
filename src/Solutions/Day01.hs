module Solutions.Day01 where

import qualified Data.Set as Set

parseInput :: [String] -> [Int]
parseInput = (parseChange <$>)
  where
    parseChange ('+':xs) = read xs
    parseChange ('-':xs) = - (read xs)
    parseChanges _       = 0

day01a :: [String] -> Int
day01a = sum . parseInput

day01b :: [String] -> Int
day01b = go Set.empty 0 . cycle . parseInput
  where go mem n (change:changes) =
          let newFreq = n + change
           in if Set.member newFreq mem
                then newFreq
                else go (Set.insert newFreq mem) newFreq changes
