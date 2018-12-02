module Day1Test where

import           Day1
import           Test.Tasty.Hspec

spec_part1 = do
  it "example 1" $ part1 ["+1", "+1", "+1"] `shouldBe` 3
  it "example 2" $ part1 ["+1", "+1", "-2"] `shouldBe` 0
  it "example 3" $ part1 ["-1", "-2", "-3"] `shouldBe` (-6)
  it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 474)

spec_part2 = do
  it "example 1" $ part2 ["+1", "-1"] `shouldBe` 0
  it "example 2" $ part2 ["+3", "+3", "+4", "-2", "-4"] `shouldBe` 10
  it "example 3" $ part2 ["-6", "+3", "+8", "+5", "-6"] `shouldBe` 5
  it "example 4" $ part2 ["+7", "+7", "-2", "-7", "-4"] `shouldBe` 14
  it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 137041)

readInput = lines <$> readFile "data/day1.csv"
