{-#LANGUAGE TupleSections #-}

module Solutions.Day03 where

import Prelude hiding (filter)

import Data.List (groupBy)
import Data.Char (isDigit)
import Data.Map.Strict as Map
import Data.Set as Set


data Rectangle = Rectangle
  { rectId :: Int
  , left   :: Int
  , top    :: Int
  , width  :: Int
  , height :: Int
  }

instance Eq Rectangle where
  (==) r1 r2 = rectId r1 == rectId r2

instance Ord Rectangle where
  compare r1 r2 = compare (rectId r1) (rectId r2)

-- #1353 @ 240,198: 29x10
parseInput :: String -> Rectangle
parseInput lines = Rectangle (read rectId) (read left) (read top) (read width) (read height)
  where
    [_, rectId, _, left, _, top, _, width, _, height] =
      groupBy (\x y -> isDigit x == isDigit y) lines

rectToMap :: Rectangle -> Map (Int, Int) Int
rectToMap (Rectangle _ l t w h) =
  Map.fromAscList $ (,1) <$> ((,) <$> [l .. l + w - 1] <*> [t .. t + h - 1])

intersect :: Rectangle -> Rectangle -> Bool
intersect (Rectangle _ l1 t1 w1 h1) (Rectangle _ l2 t2 w2 h2) | left      = False
                                                              | top       = False
                                                              | otherwise = True
  where
    left = (l1 + w1 - 1) < l2 || (l2 + w2 - 1) < l1
    top  = (t1 + h1 - 1) < t2 || (t2 + h2 - 1) < t1

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = ((x,) <$> xs) ++ (pairs xs)

day03a :: [String] -> Int
day03a = Map.size . Map.filter (> 1) . unionsWith (+) . (rectToMap <$>) . (parseInput <$>)

day03b :: [String] -> Int
day03b input = rectId $ head $ Set.toList $ Prelude.foldl dropIntersections (Set.fromList rects) (pairs rects)
  where
    rects = parseInput <$> input
    dropIntersections mem (a, b) =
      if intersect a b then
        (Set.delete a . Set.delete b) mem
      else
        mem

