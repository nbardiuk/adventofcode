{-# LANGUAGE TupleSections #-}
module Day15 where

import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Maybe                     ( catMaybes
                                                , listToMaybe
                                                , maybe
                                                , fromMaybe
                                                , fromJust
                                                , isJust
                                                )
import           Data.List                      ( sort
                                                , minimum
                                                , minimumBy
                                                , find
                                                )
import           Data.Tuple                     ( swap )
import           Control.Monad                  ( mfilter )
import           Data.Ord                       ( comparing )

part1 :: [String] -> Int
part1 ss =
  let frames = iterate (gameRound 0) $ start ss
      (i, b) = fromJust $ find (won . snd) $ zip [0 ..] frames
      hps    = sum $ hp <$> players b
  in  (i - 1) * hps

part2 :: [String] -> Int
part2 ss =
  let st    = start ss
      elves = Map.size $ Map.filter (\p -> team p == E) $ players st
      frames n = iterate (gameRound n) st
      game n = fromJust $ find (won . snd) $ zip [0 ..] $ frames n
      score (i, b) = (i - 1) * (sum $ hp <$> players b)
      elvesGame = fromJust $ find ((elvesWon elves) . snd) $ game <$> [1 ..]
  in  score elvesGame

won :: Board -> Bool
won (Board _ players) =
  (== 1) $ Set.size $ Set.fromList $ team <$> Map.elems players

elvesWon :: Int -> Board -> Bool
elvesWon original (Board _ players) =
  Map.size players == original && (all (== E) $ team <$> Map.elems players)

start :: [String] -> Board
start ss =
  let pixels  = Map.fromList $ index ss
      wall    = Map.keysSet $ Map.filter (== '#') pixels
      goblins = Map.map (const (Player G 200)) $ Map.filter (== 'G') pixels
      elfs    = Map.map (const (Player E 200)) $ Map.filter (== 'E') pixels
  in  Board wall (Map.union elfs goblins)

gameRound :: Int -> Board -> Board
gameRound extra (Board w ps) =
  let (current, next) = Map.deleteFindMin ps
  in  Board w (tick extra w (Map.empty, current, next))

tick
  :: Int
  -> Set Pos
  -> (Map Pos Player, (Pos, Player), Map Pos Player)
  -> Map Pos Player
tick extra wall (prev, current@(_, pl), next) =
  let (enemies, friends) =
        Map.partition ((/= team pl) . team) $ Map.union prev next
      moved              = move wall (enemies, friends) current
      (prevHit, nextHit) = fire extra moved enemies (prev, next)
      seen               = Map.insert moved pl prevHit
  in  if Map.null nextHit
        then seen
        else tick extra wall (seen, Map.findMin nextHit, Map.deleteMin nextHit)

fire
  :: Int
  -> Pos
  -> Map Pos Player
  -> (Map Pos Player, Map Pos Player)
  -> (Map Pos Player, Map Pos Player)
fire extra pos enemies (prev, next) =
  let targets = Map.restrictKeys enemies (neighbours pos)
      target  = minimumBy (comparing (hp . snd)) $ Map.toList targets
      hit =
        (\(pos, (Player t hp)) ->
            (pos, Player t (hp - (if t == E then 3 else (3 + extra))))
          )
          target
      prevNext = ht prev hit
      nextNext = ht next hit
  in  if Map.null targets then (prev, next) else (prevNext, nextNext)
 where
  ht m (k, v) = if hp v > 0
    then Map.update (const $ Just v) k m
    else Map.update (const Nothing) k m

move :: Set Pos -> (Map Pos Player, Map Pos Player) -> (Pos, Player) -> Pos
move wall (enemies, friends) (pos, pl) =
  let blocks = Set.unions [Set.singleton pos, wall, Map.keysSet friends]
      next   = nextPos blocks
                       (Map.keysSet enemies)
                       (Seq.singleton (Seq.singleton pos))
  in  maybe pos snd next

neighbours :: Pos -> Set Pos
neighbours p =
  let (x, y) = unpos p
  in  Set.fromList $ Pos <$> [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)]

nextPos :: Set Pos -> Set Pos -> Seq (Seq Pos) -> Maybe (Int, Pos)
nextPos blocks goals paths =
  let
    (path_, rest) = Seq.splitAt 1 paths
    path          = Seq.index path_ 0
    from          = Seq.index path (Seq.length path - 1)
    nbs           = neighbours from
    reached       = Set.intersection nbs goals
    seen          = Set.union blocks nbs
    end = Seq.fromList $ Set.toList $ Set.map (path Seq.|>) $ Set.difference
      nbs
      blocks
  in
    if Seq.null paths
      then Nothing
      else if not (Set.null reached)
        then (Seq.length path, ) <$> Seq.lookup 1 path
        else nextPos seen goals (rest Seq.>< end)

newtype Pos = Pos { unpos:: (Int, Int) } deriving (Show, Eq)
instance Ord Pos where a `compare` b = swap (unpos a) `compare` swap (unpos b) -- reading order

data Team = E | G deriving (Ord, Eq, Show)
data Player = Player {team:: Team, hp::Int} deriving (Show)
data Board = Board {wall::Set Pos, players::Map Pos Player} deriving (Show)

index :: [[a]] -> [(Pos, a)]
index ass =
  zip ass [0 ..] >>= (\(as, y) -> zipWith (\a x -> (Pos (x, y), a)) as [0 ..])
