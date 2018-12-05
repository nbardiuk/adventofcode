module Day5 where

import           Data.Char (toLower)

part1 :: String -> Int
part1 s = length $ react ("", s)

part2 :: String -> Int
part2 s = minimum $ part1 <$> dropComponent s <$> ['a' .. 'z']

-- iterates using zipper
react :: (String, String) -> String
react ([], h : t) = react ([h], t)
react (a : as, b : bs) | reacts a b = react (as, bs)
                       | otherwise  = react (b : a : as, bs)
react (s, []) = reverse s

dropComponent :: String -> Char -> String
dropComponent s c = filter (not . sameComponent c) s

reacts :: Char -> Char -> Bool
reacts a b = a /= b && toLower a == toLower b
sameComponent :: Char -> Char -> Bool
sameComponent a b = a == b || toLower a == toLower b
