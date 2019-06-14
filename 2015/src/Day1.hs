module Day1 where

import           Data.List  (elemIndex)
import           Data.Maybe (fromMaybe)

part1 :: String -> Int
part1 = sum . map direction

part2 :: String -> Int
part2 = indexOf (-1) . scanl (+) 0 . map direction

direction :: Char -> Int
direction '(' = 1
direction ')' = -1
direction _   = 0

indexOf :: Eq a => a -> [a] -> Int
indexOf i = fromMaybe (-1) . elemIndex i
