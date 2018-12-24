module Day22 where

import qualified Data.Heap                     as Heap
import qualified Data.Set                      as Set
import           Data.Function.Memoize          ( memoize )

part1 :: [String] -> Int
part1 (d : t : _) =
  let depth  = read $ drop 7 d
      tx     = read $ takeWhile (/= ',') $ drop 8 t
      ty     = read $ drop 1 $ dropWhile (/= ',') t
      region = regionType depth (tx, ty)
  in  sum $ (fromEnum . region) <$> [ (x, y) | x <- [0 .. tx], y <- [0 .. ty] ]

part2 :: [String] -> Int
part2 (d : t : _) =
  let depth  = read $ drop 7 d
      tx     = read $ takeWhile (/= ',') $ drop 8 t
      ty     = read $ drop 1 $ dropWhile (/= ',') t
      region = regionType depth (tx, ty)
  in  distance (Position Torch tx ty)
               region
               Set.empty
               (Heap.singleton (Candidate 0 0 (Position Torch 0 0)))

data Tool = None | Torch | Climbing deriving (Show, Eq, Ord)
data Region = Rocky | Wet | Narrow deriving (Enum, Show)
data Position = Position {tool:: Tool, x::Int, y::Int} deriving (Show, Eq, Ord)
data Candidate = Candidate {score::Int, dist::Int, pos:: Position} deriving (Show, Eq)
instance Ord Candidate where c1 `compare` c2 = score c1 `compare` score c2

distance
  :: Position
  -> ((Int, Int) -> Region)
  -> Set.Set Position
  -> Heap.MinHeap Candidate
  -> Int
distance target region seen heap = case Heap.view heap of
  Nothing                -> 0
  Just (candidate, rest) -> if pos candidate == target
    then dist candidate
    else
      let ns = filter (not . (`Set.member` seen) . pos)
            $ neighbours target region candidate
      in  distance target
                   region
                   (Set.insert (pos candidate) seen)
                   (foldr Heap.insert rest ns)

neighbours :: Position -> ((Int, Int) -> Region) -> Candidate -> [Candidate]
neighbours target region from =
  candidate target from <$> neis region (pos from)

candidate :: Position -> Candidate -> Position -> Candidate
candidate target from neighbour =
  let d = dist from + mDist neighbour (pos from)
      s = d + mDist neighbour target
  in  Candidate s d neighbour

neis :: ((Int, Int) -> Region) -> Position -> [Position]
neis region s =
  filter (\n -> canUse (tool n) (region (x n, y n))) -- only allowed tool for the region
    $ filter (\n -> mDist s n `elem` [1, 7]) -- only top, down or tool change
    $ filter (\n -> x n >= 0 && y n >= 0) -- only positive coordinates in the cave
    $ [ Position t a b
      | a <- [x s - 1 .. x s + 1]
      , b <- [y s - 1 .. y s + 1]
      , t <- [None, Torch, Climbing]
      ]

mDist :: Position -> Position -> Int
mDist a b =
  abs (x b - x a) + abs (y b - y a) + (if tool b == tool a then 0 else 7)

canUse :: Tool -> Region -> Bool
canUse Torch    Rocky  = True
canUse Climbing Rocky  = True
canUse None     Wet    = True
canUse Climbing Wet    = True
canUse None     Narrow = True
canUse Torch    Narrow = True
canUse _        _      = False

regionType :: Int -> (Int, Int) -> (Int, Int) -> Region
regionType depth target = toEnum . (`mod` 3) . erosionLevel depth target

erosionLevel :: Int -> (Int, Int) -> (Int, Int) -> Int
erosionLevel depth target = erosion
 where
  erosion = memoize $ (`mod` 20183) . (+ depth) . index
  index (0, 0)          = 0
  index p | p == target = 0
  index (0, y)          = y * 48271
  index (x, 0)          = x * 16807
  index (x, y)          = erosion (x - 1, y) * erosion (x, y - 1)
