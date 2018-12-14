module Day13Test where

import           Day13
import           Test.Tasty.Hspec

spec_Day13 = do

  describe "Part 1" $ do
    it "example 1" $ part1 example1 `shouldBe` (0, 3)
    it "example 2" $ part1 example2 `shouldBe` (7, 3)
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` (123, 18))

  describe "Part 2" $ do
    it "example 2" $ part2 example3 `shouldBe` (6, 4)
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` (71, 123))

example1 =
  [ "|" --
  , "v" --
  , "|" --
  , "|" --
  , "|" --
  , "^" --
  , "|" --
  ]

example2 =
  [ "/->-\\        "
  , "|   |  /----\\"
  , "| /-+--+-\\  |"
  , "| | |  | v  |"
  , "\\-+-/  \\-+--/"
  , "  \\------/   "
  ]

example3 =
  [ "/>-<\\  "
  , "|   |  "
  , "| /<+-\\"
  , "| | | v"
  , "\\>+</ |"
  , "  |   ^"
  , "  \\<->/"
  ]

readInput = lines <$> readFile "data/day13.in"
