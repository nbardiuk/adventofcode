module Day3 where

import           Data.List                  (find, inits)
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as Set
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, parseMaybe)
import           Text.Megaparsec.Char       (char, string)
import           Text.Megaparsec.Char.Lexer (decimal)

part1 :: [String] -> Int
part1 = Set.size . overlaps . (readClaim <$>)

part2 :: [String] -> Int
part2 = maybe 0 claimId . firstWithoutOverlap . cycles . (readClaim <$>)
 where
  claimId (i, _, _, _) = i
  firstWithoutOverlap = (head <$>) . find withoutOverlap
  withoutOverlap (claim : rest) = all (not . hasOverlap claim) rest
  cycles as =
    let len = length as
    in  (\n -> take len $ drop n $ cycle as) <$> [0 .. len - 1]

overlaps :: [Claim] -> Set.Set Point
overlaps claims =
  Set.unions $ uncurry ((<$>) . overlapPoints) =<< zip claims (inits claims)

overlapPoints :: Claim -> Claim -> Set.Set Point
overlapPoints c1 c2 = case c1 `overlapRect` c2 of
  ((x1, y1), (x2, y2)) -> Set.fromList [ (i, j) | i <- [x1 .. x2], j <- [y1 .. y2] ]

hasOverlap :: Claim -> Claim -> Bool
hasOverlap c1 c2 = case c1 `overlapRect` c2 of
  ((x1, y1), (x2, y2)) -> x1 <= x2 && y1 <= y2

overlapRect :: Claim -> Claim -> (Point, Point)
overlapRect (_, (x1, y1), w1, h1) (_, (x2, y2), w2, h2) =
  ( (max x1 x2                      , max y1 y2)
  , (min (x1 + w1 - 1) (x2 + w2 - 1), min (y1 + h1 - 1) (y2 + h2 - 1))
  )

type Claim = (Int, Point, Int, Int)
type Point = (Int, Int)

readClaim :: String -> Claim
readClaim = fromJust . parseMaybe claimP

claimP :: Parsec Void String Claim
claimP = do
  _ <- char '#'
  i <- decimal
  _ <- string " @ "
  x <- decimal
  _ <- char ','
  y <- decimal
  _ <- string ": "
  w <- decimal
  _ <- char 'x'
  h <- decimal
  return (i, (x, y), w, h)
