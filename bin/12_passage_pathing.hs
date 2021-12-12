import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***))
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- TODO: skip big rooms by converting connections out of big rooms
-- example, a-B B-d a-C C-d converts into two paths between a-d
-- Can save further work by multiplying results by the number of repeated edges,
-- instead of repeatedly going down the same edge.

data Room = Start | End | Big String | Small String deriving (Eq, Ord)

pathMap :: Map Room [Room] -> Map (Room, Set String, Bool) (Int, Int)
pathMap neighs = cache
  where
    cache = Map.fromList [let t = (r, Set.fromList s, b) in (t, paths t)
                         | r <- rooms, s <- powerset (smalls rooms), b <- [False, True]]
    rooms = Map.keys neighs
    paths (End, _, False) = (1, 1)
    paths (End, _, True) = (0, 1)
    paths (rm, visited, smallRepeat) = tsum (map (cache Map.!) expanded)
      where
        expanded = mapMaybe (moveIfAble visited smallRepeat) (neighs Map.! rm)

moveIfAble :: Set String -> Bool -> Room -> Maybe (Room, Set String, Bool)
moveIfAble _ _ Start = Nothing
moveIfAble visited smallRepeat (Small r) | smallRepeat && r `Set.member` visited = Nothing
moveIfAble visited smallRepeat (Small r) = Just (Small r, Set.insert r visited, smallRepeat || r `Set.member` visited)
moveIfAble visited smallRepeat (Big r) = Just (Big r, visited, smallRepeat)
moveIfAble visited smallRepeat End = Just (End, visited, smallRepeat)

smalls :: [Room] -> [String]
smalls = mapMaybe small
  where small (Small r) = Just r
        small _ = Nothing

bidirMap :: Ord a => [(a, a)] -> Map a [a]
bidirMap = Map.fromListWith (++) . concatMap pair
  where pair (a, b) = [(a, [b]), (b, [a])]

tsum :: [(Int, Int)] -> (Int, Int)
tsum = foldl' (\(a, b) (c, d) -> (a + c, b + d)) (0, 0)

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

room :: String -> Room
room "start" = Start
room "end" = End
room s | all (\c -> 'A' <= c && c <= 'Z') s = Big s
room s | all (\c -> 'a' <= c && c <= 'z') s = Small s
room s = error ("bad room: " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let neigh = bidirMap (map ((room *** room) . splitOnOne '-') (lines s))
      paths = pathMap neigh Map.! (Start, Set.empty, False)
  print (fst paths)
  print (snd paths)
