module Day5Test where

import           Day5
import           Test.Tasty.Hspec

spec_Day5 = do

  describe "Part 1" $ do
    it "example 1" $ part1 "aA" `shouldBe` 0
    it "example 2" $ part1 "abBA" `shouldBe` 0
    it "example 3" $ part1 "abAB" `shouldBe` 4
    it "example 4" $ part1 "aabAAB" `shouldBe` 6
    it "example 5" $ part1 "dabAcCaCBAcCcaDA" `shouldBe` 10
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 10250)

  describe "Part 2" $ do
    it "example 1" $ part2 "aA" `shouldBe` 0
    it "example 2" $ part2 "abBA" `shouldBe` 0
    it "example 3" $ part2 "abAB" `shouldBe` 0
    it "example 4" $ part2 "aabAAB" `shouldBe` 0
    it "example 5" $ part2 "dabAcCaCBAcCcaDA" `shouldBe` 4
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 6188)


readInput = head . lines <$> readFile "data/day5.in"
