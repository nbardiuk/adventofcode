module Day3Test where

import           Day3
import           Test.Tasty.Hspec

spec_Day3 = do

  describe "Part 1" $ do
    it "example" $ part1 example1 `shouldBe` 4
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 100595)

  describe "Part 2" $ do
    it "example" $ part2 example1 `shouldBe` 3
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 415)

example1 = ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]
readInput = lines <$> readFile "data/day3.in"
