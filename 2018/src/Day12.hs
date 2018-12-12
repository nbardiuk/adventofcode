module Day12 where


import           Data.Maybe                     ( fromJust
                                                , catMaybes
                                                )

import           Data.List                      ( iterate )
import qualified Data.IntSet                   as IntSet
import           Data.IntSet                    ( IntSet )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec
                                                , choice
                                                , parseMaybe
                                                , some
                                                )
import           Text.Megaparsec.Char           ( char
                                                , string
                                                )

part1 :: [String] -> Int
part1 input =
  let (s, t) = readIn input
      state  = IntSet.fromAscList $ (snd <$>) $ filter fst $ zip s [0 ..]
      final  = iterate (step t) state !! 20
  in  isum final

part2 :: [String] -> Int
part2 input =
  let (s, t)   = readIn input
      state    = IntSet.fromAscList $ (snd <$>) $ filter fst $ zip s [0 ..]
      steps    = iterate (step t) state
      stable   = 300 -- at this point pattern is already stable
      [n1, n2] = take 2 $ (isum <$>) $ drop stable $ steps
  in  (50000000000 - stable) * (n2 - n1) + n1

isum :: IntSet -> Int
isum = IntSet.foldl' (+) 0

step :: Transitions -> IntSet -> IntSet
step t s =
  let range = [IntSet.findMin s - 4 .. IntSet.findMax s + 4]
  in  IntSet.fromAscList $ filter alive $ range
  where alive i = (`Set.member` t) $ neighbours s i

neighbours :: IntSet -> Int -> State
neighbours s c = (`IntSet.member` s) <$> [c - 2 .. c + 2]

type Cell = Bool
type State = [Cell]
type Transition = (State, Cell)
type Transitions = Set State

readIn :: [String] -> (State, Transitions)
readIn (i : _ : tr) =
  ( fromJust $ parseMaybe initialStateP i
  , Set.fromList
    $   (fst <$>)
    $   filter snd
    $   catMaybes
    $   parseMaybe transitionP
    <$> tr
  )

cellP :: Parsec Void String Cell
cellP = choice [const True <$> char '#', const False <$> char '.']

stateP :: Parsec Void String State
stateP = some cellP

initialStateP :: Parsec Void String State
initialStateP = string "initial state: " *> stateP

transitionP :: Parsec Void String Transition
transitionP = do
  s <- stateP
  _ <- string " => "
  c <- cellP
  return (s, c)
