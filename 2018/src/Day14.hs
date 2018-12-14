module Day14 where

import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap
import           Data.Char                      ( intToDigit
                                                , digitToInt
                                                )
import           Data.List                      ( isPrefixOf
                                                , find
                                                , tails
                                                , findIndex
                                                )
import           Data.Maybe                     ( fromJust )


part1 :: Int -> String
part1 n =
  tenAfter n $ scores $ head $ dropWhile (\b -> len b < n + 10) $ iterate
    step
    start

part2 :: String -> Int
part2 s = indexWith (digitToInt <$> s) $ iterate step start

tenAfter :: Int -> IntMap Int -> String
tenAfter n scores = intToDigit <$> (take 10 $ drop n $ IntMap.elems scores)

indexWith :: [Int] -> [Board] -> Int
indexWith pattern boards =
  let rp = reverse pattern
      lp = length pattern
  in  fromJust
      $   indexOf pattern
      $   IntMap.elems
      $   fromJust
      $   find (subm rp lp)
      $   scores
      <$> boards
 where
  subm rp lp m =
    let lst = take (lp + 1) $ snd <$> IntMap.toDescList m
    in  ((rp == tail lst) || (rp == init lst))

indexOf :: [Int] -> [Int] -> Maybe Int
indexOf pattern scores = findIndex (isPrefixOf pattern) (tails scores)

data Board = Board {a::Int, b::Int, scores:: !(IntMap Int) , len::Int} deriving (Show)
start :: Board
start = Board 0 1 (IntMap.fromList $ zip [0 ..] [3, 7]) 2

step :: Board -> Board
step (Board a b scores l) =
  let nextScores = IntMap.union scores $ IntMap.fromList $ zip [l ..] digs
      len        = length digs + l
      va         = scores IntMap.! a
      vb         = scores IntMap.! b
      digs       = digits (va + vb)
  in  Board (mod (a + va + 1) len) (mod (b + vb + 1) len) nextScores len

digits i = case divMod i 10 of
  (0, m) -> [m]
  (d, m) -> [d, m]
