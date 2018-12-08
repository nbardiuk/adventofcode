module Day7Test where

import           Day7
import           Test.Tasty.Hspec

spec_Day7 = do

  describe "Part 1" $ do
    it "example 1" $ part1 example1 `shouldBe` "CABDFE"
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` "CQSWKZFJONPBEUMXADLYIGVRHT")

  describe "Part 2" $ do
    it "example 1" $ part2 2 0 example1 `shouldBe` 15
    it "my input" $ (part2 5 60 <$> readInput) >>= (`shouldBe` 914)

example1 =
  [ "Step C must be finished before step A can begin."
  , "Step C must be finished before step F can begin."
  , "Step A must be finished before step B can begin."
  , "Step A must be finished before step D can begin."
  , "Step B must be finished before step E can begin."
  , "Step D must be finished before step E can begin."
  , "Step F must be finished before step E can begin."
  ]
readInput = lines <$> readFile "data/day7.in"
