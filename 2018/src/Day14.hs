module Day14 where

import           Data.Char                      ( intToDigit
                                                , digitToInt
                                                )
import           Data.List                      ( isPrefixOf
                                                , find
                                                , tails
                                                , findIndex
                                                )
import           Data.Maybe                     ( fromJust )

import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq

import           Data.Foldable                  ( toList )

part1 :: Int -> String
part1 n =
  tenAfter n
    $ scores
    $ head
    $ dropWhile (\b -> (Seq.length $ scores b) < n + 10)
    $ iterate step start

part2 :: String -> Int
part2 s = indexWith (digitToInt <$> s) $ iterate step start

tenAfter :: Int -> Seq Int -> String
tenAfter n scores = intToDigit <$> (take 10 $ drop n $ toList scores)

indexWith :: [Int] -> [Board] -> Int
indexWith pattern boards =
  let lp = length pattern
  in  fromJust
      $   indexOf pattern
      $   toList
      $   fromJust
      $   find (subm pattern lp)
      $   scores
      <$> boards
 where
  subm p lp m =
    let lst = toList $ Seq.drop (Seq.length m - lp - 1) m
    in  (p == tail lst) || (p == init lst)

indexOf :: [Int] -> [Int] -> Maybe Int
indexOf pattern scores = findIndex (isPrefixOf pattern) (tails scores)

data Board = Board {a::Int, b::Int, scores:: !(Seq Int)} deriving (Show)
start :: Board
start = Board 0 1 (Seq.fromList [3, 7])

step :: Board -> Board
step (Board a b scores) =
  let nextScores = scores Seq.>< (Seq.fromList digs)
      len        = Seq.length nextScores
      va         = fromJust $ Seq.lookup a scores
      vb         = fromJust $ Seq.lookup b scores
      digs       = digits (va + vb)
  in  Board (mod (a + va + 1) len) (mod (b + vb + 1) len) nextScores

digits i = case divMod i 10 of
  (0, m) -> [m]
  (d, m) -> [d, m]
