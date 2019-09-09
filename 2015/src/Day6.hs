{-# LANGUAGE LambdaCase #-}

module Day6 where

import qualified Data.Array as Array
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, parseMaybe, sepBy)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)

part1 :: [String] -> Int
part1 =
  totalPower $ \case
    On -> const 1
    Off -> const 0
    Toggle -> (1 -)

part2 :: [String] -> Int
part2 =
  totalPower $ \case
    On -> (+ 1)
    Off -> \x -> max 0 (x - 1)
    Toggle -> (+ 2)

totalPower :: (Operation -> (Int -> Int)) -> [String] -> Int
totalPower update =
  sum . Array.accumArray apply 0 size . (>>= pointChanges) . readIn
  where
    pointChanges (op, rect) = [(point, update op) | point <- Array.range rect]
    apply power change = change power
    size = ((0, 0), (999, 999))

data Operation
  = On
  | Off
  | Toggle

type Rect = ((Int, Int), (Int, Int))

type Command = (Operation, Rect)

readIn :: [String] -> [Command]
readIn = (fromJust . parseMaybe commandP <$>)

commandP :: Parsec Void String Command
commandP = do
  c <-
    choice
      [ On <$ string "turn on "
      , Off <$ string "turn off "
      , Toggle <$ string "toggle "
      ]
  [ax, ay] <- decimal `sepBy` char ','
  _ <- string " through "
  [bx, by] <- decimal `sepBy` char ','
  return (c, ((ax, ay), (bx, by)))
