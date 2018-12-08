module Day8 where

import           Data.Maybe                 (catMaybes, fromJust, listToMaybe)
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, parseMaybe, some)
import           Text.Megaparsec.Char       (space)
import           Text.Megaparsec.Char.Lexer (decimal)

part1 :: String -> Int
part1 = sumMetadata . tree . readIn

part2 :: String -> Int
part2 = nodeValue . tree . readIn

sumMetadata :: Node -> Int
sumMetadata n = sum (metadata n) + sum (sumMetadata <$> children n)

nodeValue :: Node -> Int
nodeValue n = case children n of
  [] -> sum $ metadata n
  ch -> sum $ catMaybes $ index (nodeValue <$> ch) <$> metadata n
  where index as i = listToMaybe $ drop (i - 1) as

data Node = Node { children :: [Node], metadata :: [Int]} deriving Show

tree :: [Int] -> Node
tree = fst . head . node

node :: Reads Int Node
node (n : m : rest) = lift2 Node (many n node) (meta m) rest
node _            = []

meta :: Int -> Reads Int [Int]
meta m is = [splitAt m is]

type Reads i o = [i] -> [(o, [i])]

many :: Int -> Reads a b -> Reads a [b]
many 0 _ as = [([], as)]
many n p as = case p as of
  (b, rest) : _ -> case many (n - 1) p rest of
    (bs, restMany) : _ -> [(b : bs, restMany)]
    _                  -> [([b], rest)]
  _ -> [([], as)]


lift2 :: (a -> b -> c) -> Reads i a -> Reads i b -> Reads i c
lift2 f ra rb is = case ra is of
  (a, ais) : _ -> case rb ais of
    (b, rest) : _ -> [(f a b, rest)]
    _             -> []
  _ -> []

readIn :: String -> [Int]
readIn = fromJust . parseMaybe listP

listP :: Parsec Void String [Int]
listP = some $ decimal <* space
