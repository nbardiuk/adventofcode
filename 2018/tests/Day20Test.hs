module Day20Test where

import           Day20
import           Test.Tasty.Hspec

spec_Day20 = do

  describe "Part 1" $ do
    it "example 1" $ part1 "^WNE$" `shouldBe` 3
    it "example 2" $ part1 "^ENWWW(NEEE|SSE(EE|N))$" `shouldBe` 10
    it "example 3" $ part1 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$" `shouldBe` 18
    it "example 4" $ part1 "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$" `shouldBe` 23
    it "example 5" $ part1 "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$" `shouldBe` 31
    it "my input" $ (part1 <$> readInput) >>= (`shouldBe` 3966)

  describe "Part 2" $ do
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 8173)

readInput = head . lines <$> readFile "data/day20.in"
