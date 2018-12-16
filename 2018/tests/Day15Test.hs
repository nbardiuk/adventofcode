module Day15Test where

import           Day15
import           Test.Tasty.Hspec

spec_Day15 = do

  describe "Part 1" $ do
    --it "example 1" $ part1 example1 `shouldBe` 27730 -- TODO failing
    it "example 2" $ part1 example2 `shouldBe` 36334
    it "example 3" $ part1 example3 `shouldBe` 39514
    it "example 4" $ part1 example4 `shouldBe` 27755
    it "example 5" $ part1 example5 `shouldBe` 28944
    it "example 6" $ part1 example6 `shouldBe` 18740
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 224370) -- TODO runs 500 ms

  describe "Part 2" $ do
    it "example 1" $ part2 example1 `shouldBe` 4988
    it "example 3" $ part2 example3 `shouldBe` 31284
    -- it "example 4" $ part2 example4 `shouldBe` 3478 -- TODO failing
    -- it "example 5" $ part2 example5 `shouldBe` 6474 -- TODO failing
    -- it "example 6" $ part2 example6 `shouldBe` 1140 -- TODO failing
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 45539) -- TODO runs 2.5 sec

example1 =
  [ "#######" --
  , "#.G...#" --
  , "#...EG#" --
  , "#.#.#G#" --
  , "#..G#E#" --
  , "#.....#" --
  , "#######" --
  ]

example2 =
  [ "#######" --
  , "#G..#E#" --
  , "#E#E.E#" --
  , "#G.##.#" --
  , "#...#E#" --
  , "#...E.#" --
  , "#######" --
  ]

example3 =
  [ "#######" --
  , "#E..EG#" --
  , "#.#G.E#" --
  , "#E.##E#" --
  , "#G..#.#" --
  , "#..E#.#" --
  , "#######" --
  ]


example4 =
  [ "#######" --
  , "#E.G#.#" --
  , "#.#G..#" --
  , "#G.#.G#" --
  , "#G..#.#" --
  , "#...E.#" --
  , "#######" --
  ]


example5 =
  [ "#######" --
  , "#.E...#" --
  , "#.#..G#" --
  , "#.###.#" --
  , "#E#G#G#" --
  , "#...#G#" --
  , "#######" --
  ]

example6 =
  [ "#########" --
  , "#G......#" --
  , "#.E.#...#" --
  , "#..##..G#" --
  , "#...##..#" --
  , "#...#...#" --
  , "#.G...G.#" --
  , "#.....G.#" --
  , "#########" --
  ]

readInput = lines <$> readFile "data/day15.in"
