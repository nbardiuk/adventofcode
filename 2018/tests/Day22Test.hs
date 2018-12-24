module Day22Test where

import           Day22
import           Test.Tasty.Hspec

spec_Day22 = do

  describe "Part 1" $ do
    it "example 1" $ part1 ["depth: 510", "target: 10,10"] `shouldBe` 114
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 9899)
  describe "Part 2" $ do
    it "example 1" $ part2 ["depth: 510", "target: 10,10"] `shouldBe` 45
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 1051) -- TODO runs 13 seconds

readInput = lines <$> readFile "data/day22.in"
