import AdventOfCode (readInputFile)

import Control.Arrow (first)
import Data.Char (digitToInt)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (elemIndex, mapAccumL, unfoldr)
import Data.Maybe (catMaybes, fromJust)

type Octopi = IntMap Int

-- Compute single octopus update step
-- (where all octopi increment, and maybe chain reactions happen)
step :: Int -> Octopi -> (Int, Octopi)
step dy = uncurry (reflash dy) . initialFlashers
  where initialFlashers = IntMap.mapAccumWithKey (\acc k v -> if v == 9 then (k : acc, 0) else (acc, succ v)) []

-- Given octopi that are flashing,
-- update their neighbours and repeats chain reaction until no more flashes.
-- Returns number of octopi that flashed and new octopi.
reflash :: Int -> [Int] -> Octopi -> (Int, Octopi)
reflash _ [] oct = (0, oct)
reflash dy (x:xs) oct = first succ (reflash dy (catMaybes xs' ++ xs) oct')
  where (oct', xs') = mapAccumL add oct (neigh dy x)
        add o pos = (o', if prev == Just 9 then Just pos else Nothing)
          where (prev, o') = IntMap.updateLookupWithKey (\_ v -> Just (if v == 0 || v == 9 then 0 else v + 1)) pos o

neigh :: Int -> Int -> [Int]
neigh w p = [p + dy + dx | dy <- [-w, 0, w], dx <- [-1, 0, 1], dy + dx /= 0]

-- Parse octopi into "grid", returning grid, dy, and number of octopi.
-- The grid is stored with flattened coordinates.
-- Empty coordinate space is left between rows,
-- so pos - 1 is not in the map on a left edge, nor pos + 1 for a right edge.
-- This simplifies boundary checking (just check if it's in the map,
-- rather than making sure the coordinate didn't wrap to another row)
grid :: String -> (Octopi, Int, Int)
grid s = (IntMap.fromAscList (zip coords (concat g)), w + 1, w * h)
  where coords = [y * (w + 1) + x | y <- [1 .. h], x <- [1 .. w]]
        g = map (map digitToInt) (lines s)
        h = length g
        w = uniform length g

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

main :: IO ()
main = do
  s <- readInputFile
  let (oct, dy, n) = grid s
      steps = unfoldr (Just . step dy) oct
  print (sum (take 100 steps))
  print (1 + fromJust (elemIndex n steps))
