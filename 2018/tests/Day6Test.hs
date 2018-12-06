module Day6Test where

import           Day6
import           Test.Tasty.Hspec

spec_Day6 = do

  describe "Part 1" $ do
    it "example 1" $ part1 example1 `shouldBe` 17
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 3251)
  describe "Part 2" $ do
    it "example 1" $ part2 32 example1 `shouldBe` 16
    it "my input" $ (part2 10000 <$> readInput) >>= (`shouldBe` 47841)

example1 = ["1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9"]
readInput = lines <$> readFile "data/day6.in"
