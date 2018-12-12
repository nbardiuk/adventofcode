module Day11 where

import           Data.List                      ( maximumBy )
import           Data.Ord                       ( comparing )
import qualified Data.Map.Strict               as Map

part1 :: Int -> (Int, Int)
part1 gid = maximumBy (comparing $ totalPower gid)
                      [ (x, y) | x <- [1 .. 300 - 3], y <- [1 .. 300 - 3] ]

part2 :: Int -> (Int, Int, Int)
part2 gid
  = let
      ps = powerSums gid
      total x y n =
        gridVal x y ps
          - ( gridVal (x + n) y       ps
            + gridVal x       (y + n) ps
            - gridVal (x + n) (y + n) ps
            )
      corners =
        [ ((x, y, n), total x y n)
        | n <- [1 .. 300]
        , x <- [1 .. 300 - (n - 1)]
        , y <- [1 .. 300 - (n - 1)]
        ]
    in
      fst $ maximumBy (comparing snd) $ corners


powerSums gid = foldr (\k r -> Map.insert k ((cellPower gid k) + sums k r) r)
                      Map.empty
                      [ (x, y) | x <- [1 .. 300], y <- [1 .. 300] ]
 where
  sums (x, y) m =
    gridVal x (y + 1) m + gridVal (x + 1) y m - gridVal (x + 1) (y + 1) m

gridVal x y m = if x > 300 || y > 300 then 0 else m Map.! (x, y)

totalPower :: Int -> (Int, Int) -> Int
totalPower gid (x, y) =
  sum $ cellPower gid <$> [ (i, j) | i <- [x .. x + 2], j <- [y .. y + 2] ]

cellPower :: Int -> (Int, Int) -> Int
cellPower gid (x, y) =
  let rackId     = x + 10
      powerLevel = (rackId * y + gid) * rackId
  in  hundrets powerLevel - 5
  where hundrets i = (i `div` 100) `mod` 10
