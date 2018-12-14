module Day13 where

import qualified Data.Map.Strict               as Map
import           Debug.Trace

part1 :: [String] -> Pos
part1 ss = let (carts, track) = readIn ss in collision (cartMove track) carts

part2 :: [String] -> Pos
part2 ss = let (carts, track) = readIn ss in lastPos (cartMove track) carts

collision :: (Pos -> Cart -> (Pos, Cart)) -> Carts -> Pos
collision move carts = case tickCollide move carts of
  (Left  p) -> p
  (Right c) -> collision move c

lastPos :: (Pos -> Cart -> (Pos, Cart)) -> Carts -> Pos
lastPos move carts =
  let merged = tickMerge move carts
  in  if Map.size merged == 1
        then head $ Map.keys merged
        else lastPos move merged

cartMove :: Track -> Pos -> Cart -> (Pos, Cart)
cartMove tracks (x, y) (Cart h t) =
  let npos = case h of
        U -> (x, y - 1)
        D -> (x, y + 1)
        L -> (x - 1, y)
        R -> (x + 1, y)
      c        = tracks Map.! npos
      (nh, nt) = case (c, h) of
        ('+' , _) -> turn h t
        ('/' , U) -> (R, t)
        ('/' , L) -> (D, t)
        ('/' , D) -> (L, t)
        ('/' , R) -> (U, t)
        ('\\', U) -> (L, t)
        ('\\', L) -> (U, t)
        ('\\', D) -> (R, t)
        ('\\', R) -> (D, t)
        (_   , _) -> (h, t)
      turn U L = (L, U)
      turn R L = (U, U)
      turn D L = (R, U)
      turn L L = (D, U)
      turn h U = (h, R)
      turn U R = (R, L)
      turn R R = (D, L)
      turn D R = (L, L)
      turn L R = (U, L)
  in  (npos, Cart nh nt)

tickCollide :: (Pos -> Cart -> (Pos, Cart)) -> Carts -> Either Pos Carts
tickCollide move c = _tickCollide move (c, Map.empty)

_tickCollide :: (Pos -> Cart -> (Pos, Cart)) -> (Carts, Carts) -> Either Pos Carts
_tickCollide _ (l, r) | Map.null l = Right r
_tickCollide move (l, r) =
  let ((p, c), lr) = Map.deleteFindMin l
      (pn    , cn) = move p c
  in  if Map.member pn lr || Map.member pn r
        then Left pn
        else _tickCollide move (lr, Map.insert pn cn r)

tickMerge :: (Pos -> Cart -> (Pos, Cart)) -> Carts -> Carts
tickMerge move c = _tickMerge move (c, Map.empty)

_tickMerge :: (Pos -> Cart -> (Pos, Cart)) -> (Carts, Carts) -> Carts
_tickMerge _ (l, r) | Map.null l = r
_tickMerge move (l, r) =
  let ((p, c), lr) = Map.deleteFindMin l
      (pn    , cn) = move p c
  in  if Map.member pn lr || Map.member pn r
        then _tickMerge move (Map.delete pn lr, Map.delete pn r)
        else _tickMerge move (lr, Map.insert pn cn r)

type Pos = (Int,Int)
data Direction = U | D | L | R deriving (Show)
data Cart = Cart { heading:: Direction, turn:: Direction } deriving (Show)
type Carts = Map.Map Pos Cart
type Track = Map.Map Pos Char

readIn :: [String] -> (Carts, Track)
readIn ss =
  let
    cells  = index ss
    carts  = Map.map cart $ Map.fromList $ filter ((`elem` "><^v") . snd) cells
    tracks = Map.map cartToTrack $ Map.fromList $ filter ((' ' /=) . snd) cells
  in
    (carts, tracks)

cartToTrack :: Char -> Char
cartToTrack '>' = '-'
cartToTrack '<' = '-'
cartToTrack 'v' = '|'
cartToTrack '^' = '|'
cartToTrack c   = c

cart :: Char -> Cart
cart '>' = Cart R L
cart '^' = Cart U L
cart 'v' = Cart D L
cart '<' = Cart L L

index :: [[a]] -> [((Int, Int), a)]
index ass = zip ass [0 ..] >>= (\(as, y) -> zipWith (\a x -> ((x, y), a)) as [0 ..])
