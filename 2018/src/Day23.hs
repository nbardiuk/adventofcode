module Day23 where

import           Data.List                      ( maximumBy
                                                , sortOn
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec
                                                , parseMaybe
                                                )
import           Text.Megaparsec.Char           ( string
                                                , space
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal
                                                , signed
                                                )
import           Data.Ord                       ( comparing )

part1 :: [String] -> Int
part1 input =
  let bots    = readIn input
      bestBot = maximumBy (comparing rad) bots
  in  length $ filter (inRangeOf bestBot . pos) bots

part2 :: [String] -> Int
part2 input =
  let bots = readIn input in fromOrigin $ closest (boundingCube bots, bots)

closest :: (Cube, [Bot]) -> Cube
closest cubeBots =
  let cubes = iterate step [cubeBots]
  in  head $ dropWhile ((> 0) . width) $ fst . head <$> cubes

step :: [(Cube, [Bot])] -> [(Cube, [Bot])]
step = take 8 . sortOn (\(cube, bots) -> (-length bots, fromOrigin cube)) . (splitCubeBots =<<)

splitCubeBots :: (Cube, [Bot]) -> [(Cube, [Bot])]
splitCubeBots (cube, bots) =
  let cubes     = splitCube cube
      contained = (\cb -> filter (contains cb) bots) <$> cubes
  in  zip cubes contained

fromOrigin :: Cube -> Int
fromOrigin cube = distanceTo cube (Position 0 0 0)

distanceTo :: Cube -> Position -> Int
distanceTo (Position ax ay az, Position bx by bz) (Position cx cy cz) =
  (if ax > cx then ax - cx else if bx < cx then cx - bx else 0)
    + (if ay > cy then ay - cy else if by < cy then cy - by else 0)
    + (if az > cz then az - cz else if bz < cz then cz - bz else 0)

width :: Cube -> Int
width (Position ax _ _, Position bx _ _) = bx - ax

contains :: Cube -> Bot -> Bool
contains cube bot = distanceTo cube (pos bot) <= rad bot

boundingCube :: [Bot] -> Cube
boundingCube bots =
  let minx = minimum $ (\b -> x (pos b) - rad b) <$> bots
      miny = minimum $ (\b -> y (pos b) - rad b) <$> bots
      minz = minimum $ (\b -> z (pos b) - rad b) <$> bots
      maxx = maximum $ (\b -> x (pos b) + rad b) <$> bots
      maxy = maximum $ (\b -> y (pos b) + rad b) <$> bots
      maxz = maximum $ (\b -> z (pos b) + rad b) <$> bots
      maxd = maximum [maxx - minx, maxy - miny, maxz - minz]
      p2   = (2 ^) $ ceiling $ logBase 2 $ fromIntegral maxd
  in  (Position minx miny minz, Position (minx + p2) (miny + p2) (minz + p2))

splitCube :: Cube -> [Cube]
splitCube cube@(a@(Position ax ay az), b@(Position bx by bz)) =
  let mx = ax + ((bx - ax) `div` 2)
      my = ay + ((by - ay) `div` 2)
      mz = az + ((bz - az) `div` 2)
  in  if width cube == 1
        then [(a, a), (b, b)]
        else
          [ (Position ax ay az, Position mx my mz)
          , (Position ax ay mz, Position mx my bz)
          , (Position ax my az, Position mx by mz)
          , (Position ax my mz, Position mx by bz)
          , (Position mx ay az, Position bx my mz)
          , (Position mx ay mz, Position bx my bz)
          , (Position mx my az, Position bx by mz)
          , (Position mx my mz, Position bx by bz)
          ]

intersect :: Bot -> Bot -> Bool
intersect a b = distance (pos a) (pos b) <= (rad a + rad b)

inRangeOf :: Bot -> Position -> Bool
inRangeOf bot p = distance p (pos bot) <= rad bot

distance :: Position -> Position -> Int
distance a b = abs (x a - x b) + abs (y a - y b) + abs (z a - z b)

type Cube = (Position, Position)
data Bot = Bot {pos:: Position, rad::Int} deriving (Eq, Ord)
data Position = Position {x::Int, y::Int, z::Int} deriving (Show, Eq, Ord)

readIn :: [String] -> [Bot]
readIn = (readBot <$>)

readBot :: String -> Bot
readBot = fromJust . parseMaybe botP

botP :: Parsec Void String Bot
botP = do
  x <- string "pos=<" *> signed space decimal
  y <- string "," *> signed space decimal
  z <- string "," *> signed space decimal <* string ">, "
  r <- string "r=" *> decimal
  return (Bot (Position x y z) r)
