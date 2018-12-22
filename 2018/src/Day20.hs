module Day20 where

import qualified Data.Map.Strict as Map
import Data.Map.Strict  (Map)

part1 :: String -> Int
part1 rx = let m = distance rx [(0,0)] (Map.singleton (0,0) 0) in maximum $ Map.elems m

part2 :: String -> Int
part2 rx = let m = distance rx [(0,0)] (Map.singleton (0,0) 0) in length $ filter (>= 1000) $ Map.elems m

distance :: String -> [(Int,Int)] -> Map (Int,Int) Int -> Map (Int,Int) Int
distance ('E':rest) ((x,y):stack) m = let p = (x-1,y) in distance rest (p:stack) (Map.insertWith min p (1 +  m Map.! (x,y)) m)
distance ('W':rest) ((x,y):stack) m = let p = (x+1,y) in distance rest (p:stack) (Map.insertWith min p (1 +  m Map.! (x,y)) m)
distance ('N':rest) ((x,y):stack) m = let p = (x,y-1) in distance rest (p:stack) (Map.insertWith min p (1 +  m Map.! (x,y)) m)
distance ('S':rest) ((x,y):stack) m = let p = (x,y+1) in distance rest (p:stack) (Map.insertWith min p (1 +  m Map.! (x,y)) m)
distance ('(':rest) stack@(p:_) m = distance rest (p:stack) m
distance (')':rest) (_:stack) m = distance rest stack m
distance ('|':rest) (_:stack@(p:_)) m = distance rest (p:stack) m
distance (_:rest) p m = distance rest p m
distance "" _ m = m
