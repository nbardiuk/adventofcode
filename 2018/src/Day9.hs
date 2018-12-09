module Day9 where

import qualified Data.IntMap.Strict            as Map
import           Data.Maybe                     ( fromJust )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec
                                                , parseMaybe
                                                )
import           Text.Megaparsec.Char           ( string )
import           Text.Megaparsec.Char.Lexer     ( decimal )

part1 :: String -> Int
part1 = maxScore gameScores . readIn

gameScores :: [Int]
gameScores = snd <$> iterate gameStep (initBoard, 0)

maxScore :: [Int] -> (Int, Int) -> Int
maxScore scores (players, steps) =
  maximum . Map.elems $ Map.fromListWith (+) $ take (steps + 1) $ zip
    (cycle [1 .. players])
    scores

gameStep :: (Board, Int) -> (Board, Int)
gameStep (board, _) =
  let next = 1 + step board
      c    = marbles board
  in  if next `mod` 23 == 0
        then let nc = rotate (-7) c in (Board (pop nc) next, next + (focus nc))
        else (Board (append next $ rotate 1 c) next, 0)

data Board = Board {marbles:: Circle Int, step:: Int} deriving Show
initBoard = Board (Circle 0 [] 0 [] 0) 0

data Circle a = Circle {leftLen:: Int, left:: [a], focus:: a, right:: [a], rightLen:: Int} deriving Show

rotate :: Int -> Circle Int -> Circle Int
rotate n c@(Circle lc l f r rc)
  | n == 0 = c
  | len c <= abs n = rotate ((sign n) * (abs n `mod` len c)) c
  | rc < n = rotate n $ leanRight c
  | n < (-lc) = rotate n $ leanLeft c
  | 0 < n = rotate (n - 1) (Circle (lc + 1) (f : l) (head r) (tail r) (rc - 1))
  | n < 0 = rotate (n + 1) (Circle (lc - 1) (tail l) (head l) (f : r) (rc + 1))

len (Circle lc _ _ _ rc) = lc + 1 + rc
sign i = if i >= 0 then 1 else -1

append v (Circle lc l f r rc) = Circle (lc + 1) (f : l) v r rc

pop c@(Circle lc l _ r rc) =
  if rc > 0 then Circle lc l (head r) (tail r) (rc - 1) else pop $ leanRight c

-- | lean circle to the right
leanRight (Circle lc l f r rc) = Circle 0 [] f (r ++ reverse l) (lc + rc)

-- | lean circle to the left
leanLeft (Circle lc l f r rc) = Circle (lc + rc) (l ++ reverse r) f [] 0

readIn :: String -> (Int, Int)
readIn = fromJust . parseMaybe pairP

pairP :: Parsec Void String (Int, Int)
pairP = do
  players <- decimal <* string " players;"
  points  <- string " last marble is worth " *> decimal <* string " points"
  return (players, points)
