module Day1Test where

import           Data.Foldable    (for_)
import           Day1
import           Test.Tasty.Hspec

spec_Day1 = do

  describe "Part1" $ do
    for_ [ ("(())", 0)
         , ("()()", 0)
         , ("(((", 3)
         , ("(()(()(", 3)
         , ("))(((((", 3)
         , ("())", -1)
         , ("))(", -1)
         , (")))", -3)
         , (")())())", -3)
         ] (\(i, e) -> it (i <> " is " <> show e) $ part1 i `shouldBe` e)
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 280)

  describe "Part2" $ do
    for_ [ (")", 1)
         , ("()())", 5)
         ] (\(i, e) -> it (i <> " is " <> show e) $ part2 i `shouldBe` e)
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 1797)

readInput = readFile "data/day1.in"
