module Day25Test where

import           Day25
import           Test.Tasty.Hspec

spec_Day25 = do

  describe "Part 1" $ do
    it "example 1" $ part1 example1 `shouldBe` 2
    it "example 2" $ part1 example2 `shouldBe` 4
    it "example 3" $ part1 example3 `shouldBe` 3
    it "example 4" $ part1 example4 `shouldBe` 8
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 377)

example1 =
  [ "0,0,0,0"
  , "3,0,0,0"
  , "0,3,0,0"
  , "0,0,3,0"
  , "0,0,0,3"
  , "0,0,0,6"
  , "9,0,0,0"
  , "12,0,0,0"
  ]

example2 =
  [ "-1,2,2,0"
  , "0,0,2,-2"
  , "0,0,0,-2"
  , "-1,2,0,0"
  , "-2,-2,-2,2"
  , "3,0,2,-1"
  , "-1,3,2,2"
  , "-1,0,-1,0"
  , "0,2,1,-2"
  , "3,0,0,0"
  ]

example3 =
  [ "1,-1,0,1"
  , "2,0,-1,0"
  , "3,2,-1,0"
  , "0,0,3,1"
  , "0,0,-1,-1"
  , "2,3,-2,0"
  , "-2,2,0,0"
  , "2,-2,0,-1"
  , "1,-1,0,-1"
  , "3,2,0,2"
  ]

example4 =
  [ "1,-1,-1,-2"
  , "-2,-2,0,1"
  , "0,2,1,3"
  , "-2,3,-2,1"
  , "0,2,3,-2"
  , "-1,-1,1,-2"
  , "0,-2,-1,0"
  , "-2,2,3,-1"
  , "1,2,2,0"
  , "-1,-2,0,-2"
  ]

readInput = lines <$> readFile "data/day25.in"
