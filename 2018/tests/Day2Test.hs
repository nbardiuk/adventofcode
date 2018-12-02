module Day2Test where

import           Day2
import           Test.Tasty.Hspec

spec_part1 = do

  it "example" $ do
    let example =
          ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
    part1 example `shouldBe` 12

  it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 6200)

spec_part2 = do

  it "example" $ do
    let example =
          ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
    part2 example `shouldBe` "fgij"

  it "my input"
    $   (part2 <$> readInput)
    >>= (`shouldBe` "xpysnnkqrbuhefmcajodplyzw")

readInput = lines <$> readFile "data/day2.csv"
