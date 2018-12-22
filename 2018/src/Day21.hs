module Day21 where

import qualified Data.Set                      as Set
import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.List                      ( find )

part1 :: [String] -> Int
part1 _ = head bs

part2 :: [String] -> Int
part2 _ =
  let (_, i) = fromJust $ firstDuplicateBy fst $ zip bs [0 ..]
  in  head $ drop (i - 1) bs


firstDuplicateBy :: Ord e => (a -> e) -> [a] -> Maybe a
firstDuplicateBy fe as =
  (fst <$>) $ find (\(a, s) -> Set.member (fe a) s) $ withSeen as
 where
  withSeen as = zip as (seen as)
  seen = scanl (\s a -> Set.insert (fe a) s) Set.empty


--ip c                  --ip 2
--00  b = 123           --seti 123 0 1
--01  b = b & 456       --bani 1 456 1
--02  b = b == 72       --eqri 1 72 1
--03  c = b + c         --addr 1 2 2
--04  c = 0             --seti 0 0 2
--05  b = 0             --seti 0 9 1
--06  e = b | 65536     --bori 1 65536 4
--07  b = 16298264      --seti 16298264 8 1
--08  f = e & 255       --bani 4 255 5
--09  b = b + f         --addr 1 5 1
--10  b = b & 16777215  --bani 1 16777215 1
--11  b = b * 65899     --muli 1 65899 1
--12  b = b & 16777215  --bani 1 16777215 1
--13  f = 256 > e       --gtir 256 4 5
--14  c = f + c         --addr 5 2 2
--15  c = c + 1         --addi 2 1 2
--16  c = 27            --seti 27 1 2
--17  f = 0             --seti 0 3 5
--18  d = f + 1         --addi 5 1 3
--19  d = d * 256       --muli 3 256 3
--20  d = d > e         --gtrr 3 4 3
--21  c = d + c         --addr 3 2 2
--22  c = c + 1         --addi 2 1 2
--23  c = 25            --seti 25 4 2
--24  f = f + 1         --addi 5 1 5
--25  c = 17            --seti 17 1 2
--26  e = f             --setr 5 3 4
--27  c = 7             --seti 7 7 2
--28  f = b == a        --eqrr 1 0 5
--29  c = f + c         --addr 5 2 2
--30  c = 5             --seti 5 3 2
-----
-- b = 0
-- while(true):
--   e = b | 65536
--   b = 16298264
--   while(true):
--     b = (((b + e & 255) & 16777215) * 65899) & 16777215
--     if (e < 256) break
--     e = e `div` 256
--   if (b == a) break --HALT!!

bs :: [Int]
bs = drop 1 $ iterate bstep 0

bstep :: Int -> Int
bstep b = fst $ head $ drop 1 $ dropWhile (\(_, e) -> e >= 256) $ iterate
  be
  (16298264, b .|. 65536)
 where
  be (b, e) =
    ((((b + (e .&. 255)) .&. 16777215) * 65899) .&. 16777215, e `div` 256)
