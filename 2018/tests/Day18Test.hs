module Day18Test where

import           Day18
import           Test.Tasty.Hspec

spec_Day18 = do

  describe "Part 1" $ do
    it "example 1" $ part1 example1 `shouldBe` 1147
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 644640)

  describe "Part 2" $ do
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 191080) -- TODO runs > 4 sec

example1 =
  [ ".#.#...|#."
  , ".....#|##|"
  , ".|..|...#."
  , "..|#.....#"
  , "#.#|||#|#|"
  , "...#.||..."
  , ".|....|..."
  , "||...#|.#|"
  , "|.||||..|."
  , "...#.|..|."
  ]

readInput = lines <$> readFile "data/day18.in"
