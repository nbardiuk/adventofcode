module Day25 where

import           Data.Maybe                     ( fromJust )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec
                                                , parseMaybe
                                                , sepBy
                                                )
import           Text.Megaparsec.Char           ( string
                                                , space
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal
                                                , signed
                                                )
import           Data.List                      ( partition )

part1 :: [String] -> Int
part1 = length . foldr add [] . readIn

add :: Dot -> [Constelation] -> [Constelation]
add dot constelations =
  let (joined, rest) = partition (any $ (<= 3) . distance dot) constelations
  in  (dot : concat joined) : rest

distance :: Dot -> Dot -> Int
distance a b =
  abs (x a - x b) + abs (y a - y b) + abs (z a - z b) + abs (t a - t b)

type Constelation = [Dot]
data Dot = Dot {x::Int, y::Int, z::Int, t::Int}

readIn :: [String] -> [Dot]
readIn = (fromJust . parseMaybe dotP <$>)

dotP :: Parsec Void String Dot
dotP = do
  [a, b, c, d] <- signed space decimal `sepBy` string ","
  return (Dot a b c d)
