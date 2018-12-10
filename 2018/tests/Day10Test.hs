module Day10Test where

import           Day10
import           Test.Tasty.Hspec

spec_Day10 = do

  describe "Part 1" $ do
    it "example 1" $ part1 example1 `shouldBe` hi
    it "view" $ (render =<< readInput) >>= (`shouldBe` ()) -- XECXBPZB --

  describe "Part 2" $ do
    it "example 1" $ part2 example1 `shouldBe` 3
    it "my input" $ (part2 <$> readInput) >>= (`shouldBe` 10124)

hi = h ++ i
h = [ (0, y) | y <- [0 .. 7] ] ++ [ (x, 3) | x <- [1 .. 3] ] ++ [ (4, y) | y <- [0 .. 7] ]
i = [(7, 0), (7, 7)] ++ [ (8, y) | y <- [0 .. 7] ] ++ [(9, 0), (9, 7)]

example1 =
  [ "position=< 9,  1> velocity=< 0,  2>"
  , "position=< 7,  0> velocity=<-1,  0>"
  , "position=< 3, -2> velocity=<-1,  1>"
  , "position=< 6, 10> velocity=<-2, -1>"
  , "position=< 2, -4> velocity=< 2,  2>"
  , "position=<-6, 10> velocity=< 2, -2>"
  , "position=< 1,  8> velocity=< 1, -1>"
  , "position=< 1,  7> velocity=< 1,  0>"
  , "position=<-3, 11> velocity=< 1, -2>"
  , "position=< 7,  6> velocity=<-1, -1>"
  , "position=<-2,  3> velocity=< 1,  0>"
  , "position=<-4,  3> velocity=< 2,  0>"
  , "position=<10, -3> velocity=<-1,  1>"
  , "position=< 5, 11> velocity=< 1, -2>"
  , "position=< 4,  7> velocity=< 0, -1>"
  , "position=< 8, -2> velocity=< 0,  1>"
  , "position=<15,  0> velocity=<-2,  0>"
  , "position=< 1,  6> velocity=< 1,  0>"
  , "position=< 8,  9> velocity=< 0, -1>"
  , "position=< 3,  3> velocity=<-1,  1>"
  , "position=< 0,  5> velocity=< 0, -1>"
  , "position=<-2,  2> velocity=< 2,  0>"
  , "position=< 5, -2> velocity=< 1,  2>"
  , "position=< 1,  4> velocity=< 2,  1>"
  , "position=<-2,  7> velocity=< 2, -2>"
  , "position=< 3,  6> velocity=<-1, -1>"
  , "position=< 5,  0> velocity=< 1,  0>"
  , "position=<-6,  0> velocity=< 2,  0>"
  , "position=< 5,  9> velocity=< 1, -2>"
  , "position=<14,  7> velocity=<-2,  0>"
  , "position=<-3,  6> velocity=< 2, -1>"
  ]
readInput = lines <$> readFile "data/day10.in"
