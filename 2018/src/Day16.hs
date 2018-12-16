module Day16 where

import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap
import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                )
import           Data.List                      ( isPrefixOf )

part1 :: [String] -> Int
part1 input =
  let (samples, _) = readIn input
  in  (length . filter (>= 3) . (behaviors <$>)) samples

part2 :: [String] -> Int
part2 input =
  let (samples, code) = readIn input
      mapping         = resolveMapping samples
      memory          = evaluate mapping (Seq.fromList [0, 0, 0, 0]) code
  in  Seq.index memory 0

behaviors :: Sample -> Int
behaviors (before, evaluation, after) =
  length $ filter (== after) $ (\f -> eval f evaluation before) <$> allEvals

resolveMapping :: [Sample] -> IntMap Evaluation
resolveMapping samples =
  let evals = Set.fromList allEvals
      total =
        IntMap.fromListWith Set.intersection $ candidates evals <$> samples
  in  fst $ solve total

solve :: IntMap (Set Evaluation) -> (IntMap Evaluation, IntMap (Set Evaluation))
solve input =
  let (unique, rest)           = IntMap.partition ((1 ==) . Set.size) input
      solvedVals               = Set.unions $ IntMap.elems unique
      unsolved                 = IntMap.map (`Set.difference` solvedVals) rest
      (recResult, recUnsolved) = solve unsolved
      result                   = IntMap.map Set.findMin unique
  in  if IntMap.null result
        then (result, unsolved)
        else (IntMap.union result recResult, recUnsolved)

candidates :: Set Evaluation -> Sample -> (Int, Set Evaluation)
candidates evals (before, e@(i, _, _, _), after) =
  (i, Set.filter (\f -> eval f e before == after) evals)

evaluate :: IntMap Evaluation -> Memory -> [Operation] -> Memory
evaluate mapping =
  foldl (\mem e@(i, _, _, _) -> eval (mapping IntMap.! i) e mem)

type Memory = Seq Int
type Operation = (Int, Int, Int, Int)
type Sample = (Memory, Operation, Memory)
data Evaluation = Eval{ name::String, eval::Operation -> Memory -> Memory }
instance Eq Evaluation where (Eval a _) == (Eval b _) = a == b
instance Ord Evaluation where (Eval a _) `compare` (Eval b _) = a `compare` b
instance Show Evaluation where show (Eval a _) = a

allEvals :: [Evaluation]
allEvals =
  [ regreg "addr" (+)
  , regval "addi" (+)
  , regreg "mulr" (*)
  , regval "muli" (*)
  , regreg "bandr" (.&.)
  , regval "bandi" (.&.)
  , regreg "borr" (.|.)
  , regval "bori" (.|.)
  , regreg "setr" const
  , valreg "seti" const
  , valreg "gtir" (\a b -> fromEnum (a > b))
  , regreg "gtrr" (\a b -> fromEnum (a > b))
  , regval "gtri" (\a b -> fromEnum (a > b))
  , valreg "eqir" (\a b -> fromEnum (a == b))
  , regreg "eqrr" (\a b -> fromEnum (a == b))
  , regval "eqri" (\a b -> fromEnum (a == b))
  ]

regreg :: String -> (Int -> Int -> Int) -> Evaluation
regreg s f = Eval s $ \(_, ra, rb, rc) input ->
  Seq.update rc (Seq.index input ra `f` Seq.index input rb) input

regval :: String -> (Int -> Int -> Int) -> Evaluation
regval s f = Eval s
  $ \(_, ra, b, rc) input -> Seq.update rc (Seq.index input ra `f` b) input

valreg :: String -> (Int -> Int -> Int) -> Evaluation
valreg s f = Eval s
  $ \(_, a, rb, rc) input -> Seq.update rc (a `f` Seq.index input rb) input

readIn :: [String] -> ([Sample], [Operation])
readIn (bef : op : aft : rest) | "Before" `isPrefixOf` bef =
  let (ss, ops) = readIn rest in (readSample bef op aft : ss, ops)
readIn ("" : rest) = readIn rest
readIn (op : rest) = let (ss, ops) = readIn rest in (ss, readOp op : ops)
readIn []          = ([], [])

readOp :: String -> Operation
readOp op = let [o, ra, rb, rd] = read <$> words op in (o, ra, rb, rd)

readSample :: String -> String -> String -> Sample
readSample b op a = (memory b, readOp op, memory a)
  where memory = Seq.fromList . read . drop 8
