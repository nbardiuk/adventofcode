module Day24 where

import qualified Data.IntMap.Strict            as IntMap
import           Data.IntMap.Strict             ( IntMap )
import           Data.Maybe                     ( fromJust
                                                , listToMaybe
                                                )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec
                                                , choice
                                                , parseMaybe
                                                , sepBy
                                                , option
                                                )
import           Text.Megaparsec.Char           ( string )
import           Text.Megaparsec.Char.Lexer     ( decimal )
import           Control.Applicative            ( liftA2 )
import           Data.List                      ( sortOn )

part1 :: [String] -> Int
part1 input =
  let groups = IntMap.fromAscList $ zip [0 ..] $ readIn input
  in  score $ play groups

part2 :: [String] -> Int
part2 input =
  let groups     = IntMap.fromAscList $ zip [0 ..] $ readIn input
      boosts     = (`IntMap.map` groups) . boost ImmuneSystem <$> [1 ..]
      immuneWins = dropWhile hasInfection $ play <$> boosts
  in  score $ head immuneWins
 where
  boost a p g = g { damage = damage g + if army g == a then p else 0 }
  hasInfection = not . IntMap.null . IntMap.filter ((== Infection) . army)

score :: IntMap Group -> Int
score = sum . (size <$>) . IntMap.elems

play :: IntMap Group -> IntMap Group
play groups =
  let fights = iterate fight groups
      stail  = (fst <$>) $ dropWhile (uncurry (/=)) $ zip fights $ tail fights
  in  head stail

fight :: IntMap Group -> IntMap Group
fight groups =
  let ts = targets groups
      order =
        sortOn (\(a, _) -> let ga = groups IntMap.! a in (-initiative ga))
          $ IntMap.toList ts
  in  IntMap.filter ((0 <) . size) $ foldl fire groups order

fire :: IntMap Group -> (Int, Int) -> IntMap Group
fire groups (a, d) = IntMap.adjust (attack (groups IntMap.! a)) d groups

attack :: Group -> Group -> Group
attack a d = d { size = max 0 (size d - (canDeal a d `div` hp d)) }

targets :: IntMap Group -> IntMap Int
targets groups =
  let gs    = IntMap.toList groups
      order = sortOn (\(_, g) -> (-size g * damage g, -initiative g)) gs
  in  foldl
        (\r (i, g) -> maybe r (\t -> IntMap.insert i t r) $ target groups r g)
        IntMap.empty
        order

target :: IntMap Group -> IntMap Int -> Group -> Maybe Int
target groups seen ag =
  (fst <$>)
    $ listToMaybe
    $ sortOn (\(_, g) -> (-canDeal ag g, -size g * damage g, -initiative g))
    $ filter (not . (`elem` IntMap.elems seen) . fst)
    $ IntMap.toList
    $ IntMap.filter ((0 <) . canDeal ag)
    $ IntMap.filter ((army ag /=) . army) groups

canDeal :: Group -> Group -> Int
canDeal a d
  = (if deals a `elem` weak d
      then 2
      else if deals a `elem` immune d then 0 else 1
    )
    * size a
    * damage a


data Attack = Radiation
            | Bludgeoning
            | Fire
            | Slashing
            | Cold
            deriving (Show, Eq)

data Army = ImmuneSystem
          | Infection
          deriving (Show, Eq, Ord)

data Group = Group
  { size::Int
  , hp::Int
  , damage::Int
  , initiative::Int
  , deals::Attack
  , weak::[Attack]
  , immune::[Attack]
  , army::Army
  } deriving (Show, Eq)

readIn :: [String] -> [Group]
readIn input =
  let imm = readGroups ImmuneSystem $ tail $ takeWhile (not . null) input
      inf = readGroups Infection $ drop 2 $ dropWhile (not . null) input
  in  imm ++ inf

readGroups :: Army -> [String] -> [Group]
readGroups a = (readGroup a <$>)

readGroup :: Army -> String -> Group
readGroup a = fromJust . parseMaybe (groupP a)

groupP :: Army -> Parsec Void String Group
groupP ar = do
  s        <- decimal <* string " units "
  h        <- string "each with " *> decimal <* string " hit points "
  (wk, im) <- weakImmuneP
  d        <- string "with an attack that does " *> decimal <* string " "
  a        <- attackP <* string " damage "
  i        <- string "at initiative " *> decimal
  return (Group s h d i a wk im ar)


weakImmuneP :: Parsec Void String ([Attack], [Attack])
weakImmuneP = option
  ([], [])
  (  string "("
  *> choice
       [ liftA2 (\w i -> (w, i)) weakP   (option [] (string "; " *> immuneP))
       , liftA2 (\i w -> (w, i)) immuneP (option [] (string "; " *> weakP))
       ]
  <* string ") "
  )

weakP :: Parsec Void String [Attack]
weakP = string "weak to " *> (attackP `sepBy` string ", ")

immuneP :: Parsec Void String [Attack]
immuneP = string "immune to " *> (attackP `sepBy` string ", ")

attackP :: Parsec Void String Attack
attackP = choice
  [ const Radiation <$> string "radiation"
  , const Bludgeoning <$> string "bludgeoning"
  , const Fire <$> string "fire"
  , const Slashing <$> string "slashing"
  , const Cold <$> string "cold"
  ]
