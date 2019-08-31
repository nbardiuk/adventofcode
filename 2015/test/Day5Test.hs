module Day5Test where

import Data.Foldable (for_)
import Day5
import Test.Tasty.Hspec

spec_Day5 = do
  describe "Part 1" $ do
    for_
      [ ("ugknbfddgicrmopn", True)
      , ("aaa", True)
      , ("jchzalrnumimnmhp", False)
      , ("haegwjzuvuyypxyu", False)
      , ("dvszwmarrgswjxmb", False)
      ]
      (\(i, e) -> it (i <> " is " <> showNice e) $ nice1 i `shouldBe` e)
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 238)
  describe "Part 2" $ do
    for_
      [ ("qjhvhtzxzqqjkmpb", True)
      , ("xxyxx", True)
      , ("uurcxstgmygtbstg", False)
      , ("ieodomkazucvgmuy", False)
      ]
      (\(i, e) -> it (i <> " is " <> showNice e) $ nice2 i `shouldBe` e)
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 69)

showNice True = "nice"
showNice False = "naughty"

readInput = lines <$> readFile "data/day5.in"
