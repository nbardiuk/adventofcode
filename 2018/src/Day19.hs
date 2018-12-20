module Day19 where

import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                )
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                )
import           Data.Foldable                  ( toList )

part1 :: [String] -> Int
part1 input =
  let (op, code)  = readIn input
      execution   = iterate (step op code =<<) $ Just $ Seq.replicate 6 0
      halted      = fromJust $ last $ takeWhile (isJust) execution
      compensated = Seq.adjust (\p -> p - 1) op halted
  in  Seq.index compensated 0

part2 :: [String] -> Int
part2 input =
  let (op, code) = readIn input
      execution =
        iterate (step op code =<<) $ Just $ Seq.adjust (+ 1) 0 $ Seq.replicate 6 0
      paused     = fromJust $ last $ take 20 $ takeWhile (isJust) execution
      mx         = maximum $ toList paused
      sumFactors = sum $ filter (\f -> mx `mod` f == 0) $ [1 .. mx]
  in  sumFactors

step :: Int -> Code -> Memory -> Maybe Memory
step opRegister code memory =
  let op    = Seq.index memory opRegister
      instr = Seq.lookup op code
  in  (eval opRegister memory) <$> instr

eval :: Int -> Memory -> Instruction -> Memory
eval opRegister memory inst = Seq.adjust (+ 1) opRegister $ exec inst memory

exec :: Instruction -> Memory -> Memory
exec (Addr , p) = regreg (+) p
exec (Addi , p) = regval (+) p
exec (Mulr , p) = regreg (*) p
exec (Muli , p) = regval (*) p
exec (Bandr, p) = regreg (.&.) p
exec (Bandi, p) = regval (.&.) p
exec (Borr , p) = regreg (.|.) p
exec (Bori , p) = regval (.|.) p
exec (Setr , p) = regreg const p
exec (Seti , p) = valreg const p
exec (Gtir , p) = valreg (\a b -> fromEnum (a > b)) p
exec (Gtrr , p) = regreg (\a b -> fromEnum (a > b)) p
exec (Gtri , p) = regval (\a b -> fromEnum (a > b)) p
exec (Eqir , p) = valreg (\a b -> fromEnum (a == b)) p
exec (Eqrr , p) = regreg (\a b -> fromEnum (a == b)) p
exec (Eqri , p) = regval (\a b -> fromEnum (a == b)) p

regreg f (ra, rb, rc) input =
  Seq.update rc (Seq.index input ra `f` Seq.index input rb) input
regval f (ra, b, rc) input = Seq.update rc (Seq.index input ra `f` b) input
valreg f (a, rb, rc) input = Seq.update rc (a `f` Seq.index input rb) input

type Memory = Seq Int
type Code = Seq Instruction
type Instruction = (OP, (Int, Int, Int))
data OP = Addr | Addi | Mulr | Muli | Bandr | Bandi | Borr | Bori | Setr | Seti | Gtir | Gtrr | Gtri | Eqir | Eqrr | Eqri deriving (Show)

readIn :: [String] -> (Int, Code)
readIn (op : instr) = (read $ drop 4 op, readInstructins instr)

readInstructins :: [String] -> Code
readInstructins = Seq.fromList . (readInstr <$>)

readInstr :: String -> Instruction
readInstr input =
  let [code, a, b, c] = words input
  in  ((readCode code), ((read a), (read b), (read c)))

readCode :: String -> OP
readCode "addr"  = Addr
readCode "addi"  = Addi
readCode "mulr"  = Mulr
readCode "muli"  = Muli
readCode "bandr" = Bandr
readCode "bandi" = Bandi
readCode "borr"  = Borr
readCode "bori"  = Bori
readCode "setr"  = Setr
readCode "seti"  = Seti
readCode "gtir"  = Gtir
readCode "gtrr"  = Gtrr
readCode "gtri"  = Gtri
readCode "eqir"  = Eqir
readCode "eqrr"  = Eqrr
readCode "eqri"  = Eqri
