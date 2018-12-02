module Day2Test where

import           Day2
import           Test.Tasty.Hspec

spec_Day2 = do

  describe "Part 1" $ do

    it "example" $ do
      let example =
            [ "abcdef"
            , "bababc"
            , "abbcde"
            , "abcccd"
            , "aabcdd"
            , "abcdee"
            , "ababab"
            ]
      part1 example `shouldBe` 12

    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 6200)

  describe "Part 2" $ do

    it "example" $ do
      let example =
            ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
      part2 example `shouldBe` Just "fgij"

    it "my input"
      $   (part2 <$> readInput)
      >>= (`shouldBe` Just "xpysnnkqrbuhefmcajodplyzw")

readInput = lines <$> readFile "data/day2.csv"
