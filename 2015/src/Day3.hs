module Day3 where

import qualified Data.Set as Set

part1 :: String -> Int
part1 = uniqueSize . positions . map direction

part2 :: String -> Int
part2 = uniqueSize . uncurry (++) . both positions . turns . map direction

uniqueSize :: Ord a => [a] -> Int
uniqueSize = Set.size . Set.fromList

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (ax, ay) (bx, by) = (ax + bx, ay + by)

positions :: [(Int, Int)] -> [(Int, Int)]
positions = scanl add (0, 0)

both :: (a -> b) -> (a,a) -> (b,b)
both f (x, y) = (f x, f y)

turns :: [a] -> ([a],[a])
turns = foldr (\x (xs,ys) -> (x:ys,xs)) ([],[])

direction :: Char -> (Int, Int)
direction '>' = (1, 0)
direction '<' = (-1, 0)
direction '^' = (0, 1)
direction 'v' = (0, -1)
direction _   = (0, 0)
