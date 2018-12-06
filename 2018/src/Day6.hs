{-# LANGUAGE TupleSections #-}
module Day6 where

import           Data.List                  (minimumBy)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, fromJust)
import           Data.Ord                   (comparing)
import qualified Data.Set                   as Set
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, parseMaybe)
import           Text.Megaparsec.Char       (string)
import           Text.Megaparsec.Char.Lexer (decimal)

part1 :: [String] -> Int
part1 ss = maxval $ (`Map.withoutKeys` infinite) $ occupied
 where
  maxval m = maximum $ Map.elems m
  infinite = Set.fromList $ catMaybes $ closest ps <$> perimeter rect
  occupied = Map.fromListWith (+) $ (, 1) <$> (catMaybes $ closest ps <$> area rect)
  rect = enclosure ps
  ps   = readPair <$> ss

part2 :: Int -> [String] -> Int
part2 n ss = length safeArea
 where
  safeArea = filter (isSafe n ps) $ area $ enclosure ps
  ps       = readPair <$> ss

isSafe :: Int -> [Point] -> Point -> Bool
isSafe safe ps p = (sum $ distance p <$> ps) < safe

closest :: [Point] -> Point -> Maybe Point
closest ps p = if singleClosest then Just closestPoint else Nothing
 where
  closestPoint  = minimumBy (comparing (distance p)) ps
  singleClosest = 1 == (length $ filter (== minDistance) distances)
  minDistance   = minimum $ distances
  distances     = distance p <$> ps

area :: Rect -> [Point]
area ((x1, y1), (x2, y2)) = [ (i, j) | i <- [x1 .. x2], j <- [y1 .. y2] ]

perimeter :: Rect -> [Point]
perimeter ((x1, y1), (x2, y2)) =
  [ (i, j) | i <- [x1, x2], j <- [y1 .. y2] ]
    ++ [ (i, j) | i <- [x1 .. x2], j <- [y1, y2] ]

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

enclosure :: [Point] -> Rect
enclosure ps =
  let minx = minimum $ fst <$> ps
      miny = minimum $ snd <$> ps
      maxx = maximum $ fst <$> ps
      maxy = maximum $ snd <$> ps
  in  ((minx, miny), (maxx, maxy))

type Rect = (Point, Point)
type Point = (Int, Int)

readPair :: String -> Point
readPair = fromJust . parseMaybe pairP

pairP :: Parsec Void String Point
pairP = do
  a <- decimal
  _ <- string ", "
  b <- decimal
  return (a, b)
