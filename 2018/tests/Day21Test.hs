module Day21Test where

import           Day21
import           Test.Tasty.Hspec

spec_Day21 = do

  describe "Part 1" $ do
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 3345459)
  describe "Part 2" $ do
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 5857354)

readInput = lines <$> readFile "data/day21.in"
