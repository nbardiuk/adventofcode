module Day3 where

import           Data.List                  (findIndex)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust)
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, parseMaybe)
import           Text.Megaparsec.Char       (char, string)
import           Text.Megaparsec.Char.Lexer (decimal)

part1 :: [String] -> Int
part1 = countOverlaps . union . readClaims
 where
  countOverlaps = Map.size . Map.filter (> 1)
  union         = Map.unionsWith (+)

part2 :: [String] -> Int
part2 input = maybe 0 (+ 1) $ findIndex withoutOverlap unionSections
 where
  withoutOverlap = Map.null . Map.filter (> 1)
  unionSections  = Map.intersection union <$> claims
  union          = Map.unionsWith (+) claims
  claims         = readClaims input

readClaims :: [String] -> [Map.Map (Int, Int) Int]
readClaims = (Map.fromList <$>) . (points <$>) . (readClaim <$>)

type Claim = (Int, Int, Int, Int, Int)
type Point = ((Int, Int), Int)

points :: Claim -> [Point]
points (_, x, y, w, h) =
  [ ((i, j), 1) | i <- [x .. (x + w - 1)], j <- [y .. (y + h - 1)] ]

readClaim :: String -> Claim
readClaim = fromJust . parseMaybe claim

claim :: Parsec Void String Claim
claim = do
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
  return (i, x, y, w, h)
