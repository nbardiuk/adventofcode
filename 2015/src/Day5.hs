module Day5 where

import Data.List (isInfixOf)

part1 :: [String] -> Int
part1 = length . filter nice1

nice1 :: String -> Bool
nice1 s =
  let vowels = length $ filter (`elem` "aeiou") s
      hasDuplicates = or $ zipWith (==) s (tail s)
      clean =
        all (`notElem` ["ab", "cd", "pq", "xy"]) $
        zipWith (\a b -> [a, b]) s (tail s)
   in vowels >= 3 && hasDuplicates && clean

part2 :: [String] -> Int
part2 = length . filter nice2

nice2 :: String -> Bool
nice2 s = repeatsOverLetter && pairDuplicates s
  where
    repeatsOverLetter = or $ zipWith (==) s (drop 2 s)
    pairDuplicates (a:b:r) = [a, b] `isInfixOf` r || pairDuplicates (b : r)
    pairDuplicates _ = False
