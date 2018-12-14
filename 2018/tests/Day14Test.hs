module Day14Test where

import           Day14
import           Test.Tasty.Hspec

spec_Day14 = do

  describe "Part 1" $ do
    it "example 1" $ part1 9 `shouldBe` "5158916779"
    it "example 2" $ part1 5 `shouldBe` "0124515891"
    it "example 3" $ part1 18 `shouldBe` "9251071085"
    it "example 4" $ part1 2018 `shouldBe` "5941429882"
    it "my input" $ part1 637061 `shouldBe` "3138510102" -- TODO runs 1 sec

  describe "Part 2" $ do
    it "example 1" $ part2 "51589" `shouldBe` 9
    it "example 2" $ part2 "01245" `shouldBe` 5
    it "example 3" $ part2 "92510" `shouldBe` 18
    it "example 4" $ part2 "59414" `shouldBe` 2018
    it "my input" $ part2 "637061" `shouldBe` 20179081 -- TODO runs 13 sec
