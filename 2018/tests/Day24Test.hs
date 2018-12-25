module Day24Test where

import           Day24
import           Test.Tasty.Hspec

spec_Day24 = do

  describe "Part 1" $ do
    it "example 1" $ part1 example1 `shouldBe` 5216
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 10723)
  describe "Part 2" $ do
    it "example 1" $ part2 example1 `shouldBe` 51
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 5120) -- TODO runs .9 sec , try binary search

example1 =
  [ "Immune System:"
  , "17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2"
  , "989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3"
  , ""
  , "Infection:"
  , "801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1"
  , "4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4"
  ]

readInput = lines <$> readFile "data/day24.in"
