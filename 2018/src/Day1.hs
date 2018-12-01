module Day1 where

import           Data.List  (find)
import           Data.Maybe (fromJust)
import qualified Data.Set   as Set

part1 :: [String] -> Integer
part1 = foldl (flip parse) 0

part2 :: [String] -> Integer
part2 = fromJust . firstDuplicate . frequencies
  where frequencies = scanl (flip parse) 0 . cycle

parse :: String -> Integer -> Integer
parse ('+' : n) = \i -> i + read n
parse ('-' : n) = \i -> i - read n
parse _         = undefined

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = (fst <$>) . find (uncurry Set.member) . withSeen
 where
  withSeen as = zip as (seen as)
  seen = scanl (flip Set.insert) Set.empty
