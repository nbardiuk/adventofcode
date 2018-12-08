module Day8Test where

import           Day8
import           Test.Tasty.Hspec

spec_Day8 = do

  describe "Part 1" $ do
    it "example 1" $ part1 "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" `shouldBe` 138
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 46781)

  describe "Part 2" $ do
    it "example 1" $ part2 "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" `shouldBe` 66
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 21405)

readInput = (head . lines) <$> readFile "data/day8.in"
