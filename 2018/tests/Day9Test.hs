module Day9Test where

import           Day9
import           Test.Tasty.Hspec

spec_Day9 = do

  describe "Part 1" $ do
    it "example 1" $ part1 "9 players; last marble is worth 25 points" `shouldBe` 32
    it "example 2" $ part1 "10 players; last marble is worth 1618 points" `shouldBe` 8317
    it "example 3" $ part1 "13 players; last marble is worth 7999 points" `shouldBe` 146373
    it "example 4" $ part1 "17 players; last marble is worth 1104 points" `shouldBe` 2764
    it "example 5" $ part1 "21 players; last marble is worth 6111 points" `shouldBe` 54718
    it "example 6" $ part1 "30 players; last marble is worth 5807 points" `shouldBe` 37305
    it "my input1" $ part1 "468 players; last marble is worth 71010 points" `shouldBe` 374287
    --it "my input2" $ part1 "468 players; last marble is worth 7101000 points" `shouldBe` 3083412635 -- TODO runs ~ 4 sec


