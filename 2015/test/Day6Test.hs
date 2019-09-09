module Day6Test where

import Data.Foldable (for_)
import Day6
import Test.Tasty.Hspec

spec_Day6 = do
  describe "Part1" $ do
    for_
      [ ("turn on 0,0 through 999,999", 1000000)
      , ("toggle 0,0 through 999,0", 1000)
      ]
      (\(i, e) -> it (i <> " is " <> show e) $ part1 [i] `shouldBe` e)
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 377891)
  describe "Part2" $ do
    for_
      [("toggle 0,0 through 999,999", 2000000), ("turn on 0,0 through 0,0", 1)]
      (\(i, e) -> it (i <> " is " <> show e) $ part2 [i] `shouldBe` e)
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 14110788)

readInput = lines <$> readFile "data/day6.in"
