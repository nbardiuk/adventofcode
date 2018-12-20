module Day19Test where

import           Day19
import           Test.Tasty.Hspec

spec_Day19 = do

  describe "Part 1" $ do
    it "example 1" $ part1 example1 `shouldBe` 6
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 1536)

  describe "Part 2" $ do
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 17540352)

example1 =
  [ "#ip 0"
  , "seti 5 0 1"
  , "seti 6 0 2"
  , "addi 0 1 0"
  , "addr 1 2 3"
  , "setr 1 0 0"
  , "seti 8 0 4"
  , "seti 9 0 5"
  ]

readInput = lines <$> readFile "data/day19.in"
