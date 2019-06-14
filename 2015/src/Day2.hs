module Day2 where

import           Data.Maybe                 (fromJust)
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, parseMaybe, sepBy)
import           Text.Megaparsec.Char       (char)
import           Text.Megaparsec.Char.Lexer (decimal)

part1 :: [String] -> Int
part1 = sum . map wrapping . readIn

part2 :: [String] -> Int
part2 = sum . map ribbon . readIn

wrapping :: Dimentions -> Int
wrapping = (\sides -> 2 * sum sides + minimum sides) . areas

ribbon :: Dimentions -> Int
ribbon d = (minimum $ perimeters d) + bow d

areas :: Dimentions -> [Int]
areas (l, h, w) = [l*h, l*w, h*w]

perimeters :: Dimentions -> [Int]
perimeters (l, h, w) = (2 *) <$> [l+h, l+w, h+w]

bow :: Dimentions -> Int
bow (l, h, w) = l * h * w

type Dimentions = (Int, Int, Int)

readIn :: [String] -> [Dimentions]
readIn = (fromJust . parseMaybe dimentionsP <$>)

dimentionsP :: Parsec Void String Dimentions
dimentionsP = do
  [l, h, w] <- decimal `sepBy` char 'x'
  return (l, h, w)
