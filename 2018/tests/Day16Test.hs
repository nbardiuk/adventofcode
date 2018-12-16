module Day16Test where

import           Day16
import           Test.Tasty.Hspec
import qualified Data.Sequence as Seq

spec_Day16 = do

  describe "Part 1" $
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 640)

  describe "Part 2" $
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 472)

readInput = lines <$> readFile "data/day16.in"
