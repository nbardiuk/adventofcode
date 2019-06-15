module Day3Test where

import           Data.Foldable    (for_)
import           Day3
import           Test.Tasty.Hspec

spec_Day3 = do

  describe "Part1" $ do
    for_ [ (">", 2)
         , ("^>v<", 4)
         , ("^v^v^v^v^v", 2)
         ] (\(i, e) -> it (i <> " is " <> show e) $ part1 i `shouldBe` e)
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 2565)

  describe "Part2" $ do
    for_ [ ("^v", 3)
         , ("^>v<", 3)
         , ("^v^v^v^v^v", 11)
         ] (\(i, e) -> it (i <> " is " <> show e) $ part2 i `shouldBe` e)
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 2639)

readInput = readFile "data/day3.in"
