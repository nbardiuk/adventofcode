{-# LANGUAGE TupleSections #-}
module Day17 where

import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec
                                                , choice
                                                , parseMaybe
                                                )
import           Text.Megaparsec.Char           ( string )
import           Text.Megaparsec.Char.Lexer     ( decimal )
import           Data.Maybe                     ( catMaybes )

part1 :: [String] -> Int
part1 input =
  let clay            = readIn input
      bottom          = Set.findMax $ Set.map snd clay
      top             = Set.findMin $ Set.map snd clay
      (still, moving) = fall clay (Set.empty, Set.empty) bottom (500, 0)
      visible         = Set.filter ((>= top) . snd) (Set.union still moving)
  in  Set.size visible

part2 :: [String] -> Int
part2 input =
  let clay       = readIn input
      bottom     = Set.findMax $ Set.map snd clay
      top        = Set.findMin $ Set.map snd clay
      (still, _) = fall clay (Set.empty, Set.empty) bottom (500, 0)
      visible    = Set.filter ((>= top) . snd) still
  in  Set.size visible

fall :: Clay -> Water -> Int -> (Int, Int) -> Water
fall clay (still, moving) bottom (x, y) =
  let falling =
        Set.fromList
          $   takeWhile
                (not . (\p -> p `Set.member` clay || p `Set.member` still))
          $   (x, )
          <$> [y + 1 .. bottom]
      lastDrop  = Set.findMax falling
      newMoving = Set.union moving falling
  in  if bottom == snd lastDrop
        then (still, newMoving)
        else fill clay (still, newMoving) bottom lastDrop

fill :: Clay -> Water -> Int -> (Int, Int) -> Water
fill clay (still, moving) bottom (x, y) =
  let rows = scanl (\prev h -> Set.union prev $ row prev h) Set.empty [1 ..]
      (es, rws) = head $ filter (not . null . fst) $ zip (edges <$> rows) rows
      movingY = snd $ head es
      (movingRows, stillRows) = Set.partition (\p -> snd p == movingY) rws
      ess = foldl
        (\(st, mv) edge -> if Set.member edge mv
          then (st, mv)
          else
            let (s, m) = fall clay (st, mv) bottom edge
            in  (Set.union st s, Set.insert edge $ Set.union mv m)
        )
        (Set.union still stillRows, Set.union moving movingRows)
        es
  in  ess
 where
  leftEdge rw =
    let miny = Set.findMin $ Set.map snd rw
        minx = Set.findMin $ Set.map fst $ Set.filter ((== miny) . snd) rw
    in  if not (Set.null rw) && not ((minx - 1, miny) `Set.member` clay)
          then Just (minx - 1, miny)
          else Nothing
  rightEdge rw =
    let maxy = Set.findMin $ Set.map snd rw
        maxx = Set.findMax $ Set.map fst $ Set.filter ((== maxy) . snd) rw
    in  if not (Set.null rw) && not ((maxx + 1, maxy) `Set.member` clay)
          then Just (maxx + 1, maxy)
          else Nothing
  edges rw = catMaybes [leftEdge rw, rightEdge rw]
  row prev h = Set.union (side prev h ((x +) <$> [0 ..]))
                         (side prev h ((x -) <$> [0 ..]))
  side prev h xs =
    Set.fromList
      $   takeWhile
            (\(a, b) ->
              not ((a, b) `Set.member` clay)
                && (            (a, b + 1)
                   `Set.member` clay
                   ||           (a, b + 1)
                   `Set.member` still
                   ||           (a, b + 1)
                   `Set.member` prev
                   )
            )
      $   (, y - (h - 1))
      <$> xs

type Clay = Set (Int, Int)
type Water = (Set (Int, Int), Set (Int, Int))

readIn :: [String] -> Clay
readIn = Set.unions . catMaybes . (parseMaybe readP <$>)

readP :: Parsec Void String Clay
readP = choice [rowP, colP]

rowP :: Parsec Void String Clay
rowP = do
  y  <- string "y=" *> decimal
  _  <- string ", "
  xs <- string "x=" *> range
  return $ Set.fromList $ (, y) <$> xs

colP :: Parsec Void String Clay
colP = do
  x  <- string "x=" *> decimal
  _  <- string ", "
  ys <- string "y=" *> range
  return $ Set.fromList $ (x, ) <$> ys

range :: Parsec Void String [Int]
range = do
  a <- decimal
  _ <- string ".."
  b <- decimal
  return [a .. b]

renderAll :: Clay -> Water -> String
renderAll clay (still, moving) =
  let all    = Set.unions [clay, still, moving]
      (miny) = Set.findMin $ Set.map snd all
      (minx) = Set.findMin $ Set.map fst all
      (maxx) = Set.findMax $ Set.map fst all
      (maxy) = Set.findMax $ Set.map snd all
  in  unlines
      $   ((\p -> renderPoint clay (still, moving) p) <$>)
      <$> [ [ (x, y) | x <- [minx .. maxx] ] | y <- [miny .. maxy] ]

renderPoint :: Clay -> Water -> (Int, Int) -> Char
renderPoint clay (still, moving) p = if Set.member p clay
  then '#'
  else if Set.member p still
    then '~'
    else if Set.member p moving then '|' else ' '
