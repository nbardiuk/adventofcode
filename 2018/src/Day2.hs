module Day2 where

import           Data.List   (find, group, partition, sort)
import           Data.Monoid (Sum (Sum), getSum)

part1 :: [String] -> Int
part1 = getSum . uncurry (*) . foldMap ((Sum >< Sum) . stats)

stats :: String -> (Int, Int)
stats s = (has 2, has 3)
 where
  has       = fromEnum . (`elem` frequency)
  frequency = map length $ group $ sort s

infixr 8 ><
(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(f >< g) (x, y) = (f x, g y)

part2 :: [String] -> Maybe String
part2 boxes = common <$> find ((> 1) . length) (closeGroups boxes)

closeGroups :: [String] -> [[String]]
closeGroups ss = close : closeGroups rest
  where (close, rest) = partition (areClose $ head ss) ss

areClose :: String -> String -> Bool
areClose a b = differences <= 1
  where differences = length $ filter (uncurry (/=)) $ zip a b

common :: [String] -> String
common []      = ""
common (h : t) = foldr same h t
  where same a b = map fst $ filter (uncurry (==)) $ zip a b
