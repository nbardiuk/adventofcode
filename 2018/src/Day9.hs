module Day9 where

import qualified Data.IntMap.Strict            as Map
import           Data.Maybe                     ( fromJust )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec
                                                , parseMaybe
                                                )
import           Text.Megaparsec.Char           ( string )
import           Text.Megaparsec.Char.Lexer     ( decimal )

import           Data.List                      ( foldl' )

part1 :: String -> Int
part1 = maxScore . readIn

maxScore :: (Int, Int) -> Int
maxScore (players, steps) = maximum . Map.elems $ fst $ foldl'
  move
  (Map.empty, singleton 0)
  [1 .. steps]
 where
  move (score, board) i =
    let back    = rotate (-7) board
        forward = rotate 1 board
        player  = i `mod` players
    in  if i `mod` 23 == 0
          then (Map.insertWith (+) player (i + focus back) score, delete back)
          else (score, insertRight i forward)

readIn :: String -> (Int, Int)
readIn = fromJust . parseMaybe pairP

pairP :: Parsec Void String (Int, Int)
pairP = do
  players <- decimal <* string " players;"
  points  <- string " last marble is worth " *> decimal <* string " points"
  return (players, points)

-----------------------------------------------------------------------------------
-- Circle

data Circle a = Circle {leftLen:: Int, left:: [a], focus:: a, right:: [a], rightLen:: Int}

singleton :: a -> Circle a
singleton a = Circle 0 [] a [] 0

rotate :: Int -> Circle a -> Circle a
rotate n c@(Circle lc l f r rc)
  | n == 0 = c
  | len c <= abs n = rotate (sign n * (abs n `mod` len c)) c
  | rc < n = rotate n $ leanRight c
  | n < (-lc) = rotate n $ leanLeft c
  | 0 < n = rotate (n - 1) (Circle (lc + 1) (f : l) (head r) (tail r) (rc - 1))
  | n < 0 = rotate (n + 1) (Circle (lc - 1) (tail l) (head l) (f : r) (rc + 1))

len :: Circle a -> Int
len (Circle lc _ _ _ rc) = lc + 1 + rc

sign :: Int -> Int
sign i = if i >= 0 then 1 else -1

insertRight :: a -> Circle a -> Circle a
insertRight v (Circle lc l f r rc) = Circle (lc + 1) (f : l) v r rc

delete :: Circle a -> Circle a
delete c@(Circle lc l _ r rc) = if rc > 0
  then Circle lc l (head r) (tail r) (rc - 1)
  else delete $ leanRight c

-- | lean circle to the right
leanRight :: Circle a -> Circle a
leanRight (Circle lc l f r rc) = Circle 0 [] f (r ++ reverse l) (lc + rc)

-- | lean circle to the left
leanLeft :: Circle a -> Circle a
leanLeft (Circle lc l f r rc) = Circle (lc + rc) (l ++ reverse r) f [] 0

-----------------------------------------------------------------------------------
