module Day7 where


import           Data.Char                  (ord)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as Set
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, parseMaybe)
import           Text.Megaparsec.Char       (letterChar, string)

part1 :: [String] -> String
part1 = orderedBFS . graph . (readDep <$>)

part2 :: Int -> Int -> [String] -> Int
part2 workers secs input = parallelTime workers secs $ graph $ (readDep <$>) input

orderedBFS :: Map.Map Char (Set.Set Char) -> [Char]
orderedBFS gr = maybe [] (\h -> h : orderedBFS (Map.delete h gr)) (dagHead gr)

parallelTime :: Int -> Int -> Map.Map Char (Set.Set Char) -> Int
parallelTime workers secs gr = loop Map.empty gr
 where
  time c = secs + (ord c - ord 'A') + 1
  loop w g
    = let
        (done, progress) = Map.partition (== 0) $ Map.map (subtract 1) w
        remaining        = Map.withoutKeys g $ Map.keysSet done
        free             = workers - Map.size progress
        work =
          Map.union progress $ Map.fromSet time $ Set.take free $ Set.difference
            (dagHeads remaining)
            (Map.keysSet progress)
      in
        if Map.null work && Map.null remaining
          then 0
          else 1 + loop work remaining

dagHead :: Map.Map Char (Set.Set Char) -> Maybe Char
dagHead m =
  fst <$> (Map.lookupMin $ Map.withoutKeys m $ Set.unions $ Map.elems m)

dagHeads :: Map.Map Char (Set.Set Char) -> Set.Set Char
dagHeads m = Map.keysSet $ Map.withoutKeys m $ Set.unions $ Map.elems m

graph :: [(Char, Char)] -> Map.Map Char (Set.Set Char)
graph =
  Map.fromListWith Set.union
    . ((\(a, b) -> [(a, Set.singleton b), (b, Set.empty)]) =<<)

readDep :: String -> (Char, Char)
readDep = fromJust . parseMaybe depP

depP :: Parsec Void String (Char, Char)
depP = do
  _ <- string "Step "
  a <- letterChar
  _ <- string " must be finished before step "
  b <- letterChar
  _ <- string " can begin."
  return (a, b)
