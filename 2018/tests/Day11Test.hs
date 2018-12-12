module Day11Test where

import           Day11
import           Test.Tasty.Hspec

spec_Day11 = do

  describe "Power level" $ do
    it "8 (3,5)" $ cellPower 8 (3, 5) `shouldBe` 4
    it "57 (122,79)" $ cellPower 57 (122, 79) `shouldBe` (-5)
    it "39 (217,196)" $ cellPower 39 (217, 196) `shouldBe` 0
    it "71 (101,153)" $ cellPower 71 (101, 153) `shouldBe` 4

  describe "Part 1" $ do
    it "example 1" $ part1 18 `shouldBe` (33, 45)
    it "example 2" $ part1 42 `shouldBe` (21, 61)
    it "my input" $ part1 7511 `shouldBe` (21, 22)

  -- TODO runs ~ 5 sec
  describe "Part 2" $ do
    -- it "example 1" $ part2 18 `shouldBe` (90, 269, 16)
    -- it "example 2" $ part2 42 `shouldBe` (232, 251, 12)
    it "my input" $ part2 7511 `shouldBe` (235, 288, 13)
