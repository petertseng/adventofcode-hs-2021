import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Data.Either (partitionEithers)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map

type Interval = (Int, Int)
type Box = (Interval, Interval, Interval)
data Command = On | Off deriving (Eq, Show)

-- inclusion-exclusion
combineBox :: Map Box Int -> (Command, Box) -> Map Box Int
combineBox boxes (onoff, b) = Map.filter (/= 0) updated
  where (deletes, updates) = partitionEithers (mapMaybe check (Map.assocs boxes))
        check (k, _) | b `boxSuper` k = Just (Left k)
        check (k, v) = let inter = boxInter k b in if boxEmpty inter then Nothing else Just (Right (inter, -v))
        deleted = foldl' (flip Map.delete) boxes deletes
        updates' = if onoff == On then (b, 1) : updates else updates
        updated = Map.unionWith (+) deleted (Map.fromListWith (+) updates')

boxInter :: Box -> Box -> Box
boxInter (x1, y1, z1) (x2, y2, z2) = (interInter x1 x2, interInter y1 y2, interInter z1 z2)

boxSize :: Box -> Int
boxSize (x, y, z) = interSize x * interSize y * interSize z

boxEmpty :: Box -> Bool
boxEmpty (x, y, z) = interEmpty x || interEmpty y || interEmpty z

boxSuper :: Box -> Box -> Bool
(x1, y1, z1) `boxSuper` (x2, y2, z2) = x1 `interSuper` x2 && y1 `interSuper` y2 && z1 `interSuper` z2

interInter :: Interval -> Interval -> Interval
interInter (a, b) (c, d) = (max a c, min b d)

interSize :: Interval -> Int
interSize (a, b) | a > b = 0
interSize (a, b) = b - a + 1

interEmpty :: Interval -> Bool
interEmpty (a, b) = a > b

interSuper :: Interval -> Interval -> Bool
(a, b) `interSuper` (c, d) = a <= c && d <= b

boxinst :: String -> (Command, Box)
boxinst s = case words s of
  ["on", b] -> (On, box b)
  ["off", b] -> (Off, box b)
  _ -> error ("bad inst " ++ s)

box :: String -> Box
box s = case splitOn ',' s of
  ['x':'=':x, 'y':'=':y, 'z':'=':z] -> (range x, range y, range z)
  _ -> error ("bad box " ++ s)
  where range s' = case splitOn '.' s' of
          [mn, mx] -> (read mn, read mx)
          x -> error ("bad range " ++ show x ++ " in " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let insts = map boxinst (lines s)
      r50 = (-50, 50)
      c50 = (r50, r50, r50)
      boxPolarity = Map.assocs (foldl' combineBox Map.empty insts)
  print (sum [boxSize (boxInter b c50) * p | (b, p) <- boxPolarity])
  print (sum [boxSize b * p | (b, p) <- boxPolarity])
