module Day4Test where

import           Data.Foldable    (for_)
import           Day4
import           Test.Tasty.Hspec

spec_Day1 = do

  describe "Part1" $ for_ 
        [ ("abcdef", 609043)
        , ("pqrstuv", 1048970)
        , ("ckczppom", 117946)
        ] (\(i, e) -> it (i <> " is " <> show e) $ part1 i `shouldBe` e)

  describe "Part2" $ for_ 
        [("ckczppom", 3938038)
        ] (\(i, e) -> it (i <> " is " <> show e) $ part2 i `shouldBe` e)
