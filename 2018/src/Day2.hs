module Day2 where

import           Data.Char          (ord)
import qualified Data.IntMap.Strict as IntMap
import           Data.List          (partition)

part1 :: [String] -> Int
part1 = uncurry (*) . add . map stats
  where add = foldl (\(a, b) (c, d) -> (a + c, b + d)) (0, 0)

stats :: String -> (Int, Int)
stats s = (hasValue 2, hasValue 3)
 where
  hasValue n = if IntMap.null $ IntMap.filter (== n) frequency then 0 else 1
  frequency = IntMap.fromListWith (+) . map (\c -> (ord c, 1)) $ s

part2 :: [String] -> String
part2 []    = ""
part2 boxes = commonIn $ head $ filter ((> 1) . length) $ closeGroups boxes

closeGroups :: [String] -> [[String]]
closeGroups boxes = close : closeGroups rest
  where (close, rest) = partition (areClose $ head boxes) boxes

areClose :: String -> String -> Bool
areClose (a : as) (b : bs) | a == b = areClose as bs
                           | a /= b = as == bs
areClose "" "" = True
areClose _  _  = False

commonIn :: [String] -> String
commonIn []      = ""
commonIn [s    ] = s
commonIn (h : t) = foldr commonOf h t
  where commonOf a b = map fst $ filter (uncurry (==)) $ zip a b
