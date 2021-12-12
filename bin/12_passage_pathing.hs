{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***), first)
import Data.Bits ((.|.), bit, testBit)
import Data.List (foldl')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

data Room = Start | End | Small String | Big String deriving (Eq, Ord)

-- because of sort order:
startId, endId :: Int
startId = 0
endId = 1

compressBig :: Map Room [Room] -> Map Room (Map Room Int)
compressBig neigh = Map.fromListWith (Map.unionWith (+)) (smallToSmall ++ bigToSmall)
  where smallToSmall = [(k, Map.fromList (map (, 1) (filter endOrSmall vs)))
                       | (k, vs) <- Map.assocs neigh, not (big k)]
        -- each pair of rooms connected to a big,
        -- (including a room to itself, once)
        -- but not if the destination is the start
        bigToSmall = [ (u, Map.singleton v 1)
                     | (k, vs) <- Map.assocs neigh, u <- vs, v <- vs, v /= Start, big k]
        big (Big _) = True
        big _ = False
        endOrSmall (Small _) = True
        endOrSmall End = True
        endOrSmall _ = False

assignIds :: [Room] -> Map Room (Map Room Int) -> IntMap (IntMap Int)
assignIds rooms neigh =
  IntMap.fromAscList [(ids Map.! k, IntMap.fromAscList (map (first (ids Map.!)) (Map.assocs vs))) | (k, vs) <- Map.assocs neigh]
  where ids = Map.fromAscList (zip rooms [0..])

pathMap :: IntMap (IntMap Int) -> Map (Int, Int, Bool) (Int, Int)
pathMap neighs = cache
  where
    cache = Map.fromList [let t = (r, s, b) in (t, paths t)
                         | r <- rooms, s <- [0 .. bit (IntMap.size neighs) - 1], b <- [False, True]]
    rooms = IntMap.keys neighs
    paths (rm, _, False) | rm == endId = (1, 1)
    paths (rm, _, True) | rm == endId = (0, 1)
    paths (rm, visited, smallRepeat) = tsum (map (\(v, mul) -> ((* mul) *** (* mul)) (cache Map.! v)) expanded)
      where
        expanded = mapMaybe (moveIfAble visited smallRepeat) (IntMap.assocs (neighs IntMap.! rm))

moveIfAble :: Int -> Bool -> (Int, Int) -> Maybe ((Int, Int, Bool), Int)
moveIfAble visited smallRepeat (r, mul) | r == endId = Just ((endId, visited, smallRepeat), mul)
moveIfAble visited smallRepeat (r, _) | smallRepeat && visited `testBit` r = Nothing
moveIfAble visited smallRepeat (r, mul) = Just ((r, visited .|. bit r, smallRepeat || visited `testBit` r), mul)

bidirMap :: Ord a => [(a, a)] -> Map a [a]
bidirMap = Map.fromListWith (++) . concatMap pair
  where pair (a, b) = [(a, [b]), (b, [a])]

tsum :: [(Int, Int)] -> (Int, Int)
tsum = foldl' (\(a, b) (c, d) -> (a + c, b + d)) (0, 0)

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
      compNeigh = compressBig neigh
      intNeigh = assignIds (Map.keys neigh) compNeigh
      paths = pathMap intNeigh Map.! (startId, 0, False)
  --print compNeigh
  --print intNeigh
  print (fst paths)
  print (snd paths)
