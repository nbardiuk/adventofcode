module Day4 where

import           Data.IntMap.Strict         (IntMap)
import qualified Data.IntMap.Strict         as Map
import           Data.List                  (group, maximum, maximumBy, sort)
import           Data.Maybe                 (fromJust)
import           Data.Ord                   (comparing)
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, choice, parseMaybe, some)
import           Text.Megaparsec.Char       (char, digitChar, string)
import           Text.Megaparsec.Char.Lexer (decimal)

part1 :: [String] -> Int
part1 = signature . maximumBy comparingSum . sleepHours

part2 :: [String] -> Int
part2 = signature . maximumBy comparingMax . sleepHours

signature :: Schedule -> Int
signature (Schedule guard time) = guard * hourMax
 where
  mx      = maxval 0 time
  hourMax = head $ Map.keys $ Map.filter (== mx) time

comparingSum :: Schedule -> Schedule -> Ordering
comparingSum = comparing (\(Schedule _ t) -> Map.foldl' (+) 0 t)

comparingMax :: Schedule -> Schedule -> Ordering
comparingMax = comparing (\(Schedule _ t) -> maxval 0 t)

maxval :: Ord p => p -> IntMap p -> p
maxval d m = if Map.null m then d else maximum $ Map.elems m

sleepHours :: [String] -> [Schedule]
sleepHours = (merge <$>) . group . sort . schedules . (readLog <$>) . sort

merge :: [Schedule] -> Schedule
merge ss@((Schedule n _) : _) = Schedule n (Map.unionsWith (+) ((\(Schedule _ d) -> d) <$> ss))
merge _ = undefined

schedules :: [Log] -> [Schedule]
schedules logs = case (many schedule) logs of
  (ss, _) : _ -> ss
  _           -> []

schedule :: Reads Log Schedule
schedule = lift2 nightShift begins (many duration)
  where nightShift guard durations = Schedule guard (Map.unionsWith (+) durations)

duration :: Reads Log (Map.IntMap Int)
duration = lift2 timeRange fall woke
  where timeRange start end = Map.fromList [ (minute, 1) | minute <- [start .. end - 1] ]

begins :: Reads Log Int
begins ((Log _ (Begins n)) : rest) = [(n, rest)]
begins _                           = []

fall :: Reads Log Int
fall ((Log (_, i) FallsAsleep) : rest) = [(i, rest)]
fall _                                 = []

woke :: Reads Log Int
woke ((Log (_, i) WakesUp) : rest) = [(i, rest)]
woke _                             = []

data Action = Begins Int | FallsAsleep | WakesUp deriving Show
type Time = (Int, Int)
data Log = Log Time Action deriving Show

data Schedule = Schedule Int (Map.IntMap Int) deriving Show
instance Eq Schedule where
  (Schedule a _) == (Schedule b _) = a == b
instance Ord Schedule where
  (Schedule a _) `compare` (Schedule b _) = a `compare` b

type Reads i o = [i] -> [(o, [i])]

many :: Reads a b -> Reads a [b]
many p as = case p as of
  (b, rest) : _ -> case many p $ rest of
    (bs, restMany) : _ -> [(b : bs, restMany)]
    _                  -> [([b], rest)]
  _ -> [([], as)]

lift2 :: (a -> b -> c) -> Reads i a -> Reads i b -> Reads i c
lift2 f ra rb = \is -> case ra is of
  (a, ais) : _ -> case rb ais of
    (b, rest) : _ -> [(f a b, rest)]
    _             -> []
  _ -> []

readLog :: String -> Log
readLog = fromJust . parseMaybe logParser

logParser :: Parsec Void String Log
logParser = do
  _      <- char '['
  _      <- some digitChar
  _      <- char '-'
  _      <- some digitChar
  _      <- char '-'
  _      <- some digitChar
  _      <- char ' '
  h      <- decimal
  _      <- char ':'
  m      <- decimal
  _      <- char ']'
  _      <- char ' '
  action <- choice
    [ const FallsAsleep <$> string "falls asleep"
    , const WakesUp <$> string "wakes up"
    , Begins <$> (string "Guard #" *> decimal <* (string " begins shift"))
    ]
  return (Log (h, m) action)
