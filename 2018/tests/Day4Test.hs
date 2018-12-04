module Day4Test where

import           Day4
import           Test.Tasty.Hspec

spec_Day4 = do

  describe "Part 1" $ do
    it "example" $ part1 example1 `shouldBe` 240
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 125444)

  describe "Part 2" $ do
    it "example" $ part2 example1 `shouldBe` 4455
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 18325)

example1 =
  [ "[1518-11-01 00:00] Guard #10 begins shift"
  , "[1518-11-01 00:05] falls asleep"
  , "[1518-11-01 00:25] wakes up"
  , "[1518-11-01 00:30] falls asleep"
  , "[1518-11-01 00:55] wakes up"
  , "[1518-11-01 23:58] Guard #99 begins shift"
  , "[1518-11-02 00:40] falls asleep"
  , "[1518-11-02 00:50] wakes up"
  , "[1518-11-03 00:05] Guard #10 begins shift"
  , "[1518-11-03 00:24] falls asleep"
  , "[1518-11-03 00:29] wakes up"
  , "[1518-11-04 00:02] Guard #99 begins shift"
  , "[1518-11-04 00:36] falls asleep"
  , "[1518-11-04 00:46] wakes up"
  , "[1518-11-05 00:03] Guard #99 begins shift"
  , "[1518-11-05 00:45] falls asleep"
  , "[1518-11-05 00:55] wakes up"
  ]
readInput = lines <$> readFile "data/day4.log"
