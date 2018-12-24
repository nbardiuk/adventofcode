module Day23Test where

import           Day23
import           Test.Tasty.Hspec

spec_Day23 = do

  describe "Part 1" $ do
    it "example 1" $ part1 example1 `shouldBe` 7
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 481)

  describe "Part 2" $ do
    it "example 2" $ part2 example2 `shouldBe` 36
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 47141479)

example1 =
  [ "pos=<0,0,0>, r=4"
  , "pos=<1,0,0>, r=1"
  , "pos=<4,0,0>, r=3"
  , "pos=<0,2,0>, r=1"
  , "pos=<0,5,0>, r=3"
  , "pos=<0,0,3>, r=1"
  , "pos=<1,1,1>, r=1"
  , "pos=<1,1,2>, r=1"
  , "pos=<1,3,1>, r=1"
  ]

example2 =
  [ "pos=<10,12,12>, r=2"
  , "pos=<12,14,12>, r=2"
  , "pos=<16,12,12>, r=4"
  , "pos=<14,14,14>, r=6"
  , "pos=<50,50,50>, r=200"
  , "pos=<10,10,10>, r=5"
  ]

readInput = lines <$> readFile "data/day23.in"
