module Day17Test where

import           Day17
import           Test.Tasty.Hspec

spec_Day17 = do

  describe "Part 1" $ do
    it "example 1" $ part1 example1 `shouldBe` 57
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 33362)
  describe "Part 2" $ do
    it "example 1" $ part2 example1 `shouldBe` 29
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 27801)

example1 =
  [ "x=495, y=2..7"
  , "y=7, x=495..501"
  , "x=501, y=3..7"
  , "x=498, y=2..4"
  , "x=506, y=1..2"
  , "x=498, y=10..13"
  , "x=504, y=10..13"
  , "y=13, x=498..504"
  ]


readInput = lines <$> readFile "data/day17.in"
