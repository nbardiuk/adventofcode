module Day1 where

import qualified Data.IntSet as Set
import           Data.List   (find)
import           Data.Maybe  (fromJust)

part1 :: [String] -> Int
part1 = sum . map parseInt

part2 :: [String] -> Int
part2 = fromJust . firstDuplicate . frequencies
  where frequencies = scanl (+) 0 . cycle . map parseInt

parseInt :: String -> Int
parseInt ('+' : n) = read n
parseInt n         = read n

firstDuplicate :: [Int] -> Maybe Int
firstDuplicate = (fst <$>) . find (uncurry Set.member) . withSeen
 where
  withSeen as = zip as (seen as)
  seen = scanl (flip Set.insert) Set.empty
