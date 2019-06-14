module Day2Test where

import           Data.Foldable    (for_)
import           Day2
import           Test.Tasty.Hspec

spec_Day2 = do

  describe "Part1" $ do
    for_ [ (["2x3x4"], 58)
         , (["1x1x10"], 43)
         ] (\(i, e) -> it ((i !! 0) <> " is " <> show e) $ part1 i `shouldBe` e)
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 1606483)

  describe "Part2" $ do
    for_ [ (["2x3x4"], 34)
         , (["1x1x10"], 14)
         ] (\(i, e) -> it ((i !! 0) <> " is " <> show e) $ part2 i `shouldBe` e)
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 3842356)


readInput = lines <$> readFile "data/day2.in"
