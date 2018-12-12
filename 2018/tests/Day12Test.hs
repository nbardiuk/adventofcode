module Day12Test where

import           Day12
import           Test.Tasty.Hspec

spec_Day12 = do

  describe "Part 1" $ do
    it "example 1" $ part1 example1 `shouldBe` 325
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 2542)

  describe "Part 2" $ do
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 2550000000883)

example1 =
  [ "initial state: #..#.#..##......###...###"
  , ""
  , "...## => #"
  , "..#.. => #"
  , ".#... => #"
  , ".#.#. => #"
  , ".#.## => #"
  , ".##.. => #"
  , ".#### => #"
  , "#.#.# => #"
  , "#.### => #"
  , "##.#. => #"
  , "##.## => #"
  , "###.. => #"
  , "###.# => #"
  , "####. => #"
  ]

readInput = lines <$> readFile "data/day12.in"
