module Day18 where

import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.List                      ( find )
import           Data.Maybe                     ( fromJust )

part1 :: [String] -> Int
part1 input =
  let cells   = Map.map readCell $ Map.fromList $ index input
      after10 = head $ drop 10 $ iterate step cells
  in  score after10

part2 :: [String] -> Int
part2 input =
  let cells   = Map.map readCell $ Map.fromList $ index input
      steps   = iterate step cells
      (_, fd) = fromJust $ firstDuplicateBy fst $ zip steps [0 ..]
      (_, sd) = fromJust $ firstDuplicateBy fst $ drop (fd) $ zip steps [0 ..]
      goal    = fd + ((1000000000 - fd) `mod` (sd - fd))
      result  = head $ drop goal steps
  in  score result

score :: Map (Int, Int) Cell -> Int
score m =
  let trees   = Map.size $ Map.filter (== Trees) m
      lambers = Map.size $ Map.filter (== Lamberyard) m
  in  (trees * lambers)


firstDuplicateBy :: Ord e => (a -> e) -> [a] -> Maybe a
firstDuplicateBy fe as =
  (fst <$>) $ find (\(a, s) -> Set.member (fe a) s) $ withSeen as
 where
  withSeen as = zip as (seen as)
  seen = scanl (\s a -> Set.insert (fe a) s) Set.empty

step :: Map (Int, Int) Cell -> Map (Int, Int) Cell
step m = Map.mapWithKey (\p c -> nextState c $ neighbours m p) m

nextState :: Cell -> [Cell] -> Cell
nextState Open ns =
  if 3 <= (length $ filter (== Trees) ns) then Trees else Open
nextState Trees ns =
  if 3 <= (length $ filter (== Lamberyard) ns) then Lamberyard else Trees
nextState Lamberyard ns =
  if 1
       <= (length $ filter (== Trees) ns)
       && 1
       <= (length $ filter (== Lamberyard) ns)
    then Lamberyard
    else Open

neighbours :: Map (Int, Int) Cell -> (Int, Int) -> [Cell]
neighbours m (x, y) =
  Map.elems $ Map.restrictKeys m $ Set.delete (x, y) $ Set.fromList
    [ (a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1] ]

data Cell = Open | Trees | Lamberyard deriving (Show, Eq, Ord)

readCell :: Char -> Cell
readCell '|' = Trees
readCell '#' = Lamberyard
readCell _   = Open

index :: [[a]] -> [((Int, Int), a)]
index ass =
  zip ass [0 ..] >>= (\(as, y) -> zipWith (\a x -> ((x, y), a)) as [0 ..])
