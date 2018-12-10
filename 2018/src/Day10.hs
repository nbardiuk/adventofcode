module Day10 where

import           Data.Maybe                     ( fromJust )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec
                                                , parseMaybe
                                                )
import           Text.Megaparsec.Char           ( string
                                                , char
                                                , space
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal
                                                , signed
                                                )
import           Data.List                      ( sort )

part1 :: [String] -> [Loc]
part1 input =
  let points = readIn <$> input
  in  sort $ fst <$> afterSteps (stepWithText points) <$> points

part2 :: [String] -> Int
part2 = stepWithText . (readIn <$>)

render :: [String] -> IO ()
render s = do
  let
    lights = part1 s
    minx   = minimum $ fst <$> lights
    miny   = minimum $ snd <$> lights
    maxx   = maximum $ fst <$> lights
    maxy   = maximum $ snd <$> lights
    view    = [ [ (x, y) | x <- [minx .. maxx] ] | y <- [miny .. maxy] ]
    text = ((\light -> if light `elem` lights then '#' else '.') <$>) <$> view
  _ <- print ""
  _ <- traverse print text
  return ()

stepWithText :: [Point] -> Int
stepWithText ps = localMinBy (heightn ps) 3 10

localMinBy :: Ord b => (Int -> b) -> Int -> Int -> Int
localMinBy f start step
  | step == 0                   = start
  | f (start + step) < f start  = localMinBy f (start + step) step
  | f start < f (start + step)  = localMinBy f start (step - 1)
  | f start == f (start + step) = localMinBy f (start - 1) step

heightn :: [Point] -> Int -> Int
heightn ps n = let ys = yN n <$> ps in maximum ys - minimum ys

yN :: Int -> Point -> Int
yN n ((_, y), (_, vy)) = y + n * vy


afterSteps :: Int -> Point -> Point
afterSteps n ((x, y), v@(vx, vy)) = ((x + n * vx, y + n * vy), v)

type Loc = (Int, Int)
type Vel = (Int, Int)
type Point = (Loc, Vel)

readIn :: String -> Point
readIn = fromJust . parseMaybe pointP

pointP :: Parsec Void String Point
pointP = do
  _  <- string "position=<"
  x  <- space *> signed space decimal <* char ','
  y  <- space *> signed space decimal <* char '>'
  _  <- string " velocity=<"
  vx <- space *> signed space decimal <* char ','
  vy <- space *> signed space decimal <* char '>'
  return ((x, y), (vx, vy))
