import AdventOfCode (readInputFile)
import AdventOfCode.Search (astarInt)

import Data.Array.Unboxed ((!), UArray, bounds, inRange, listArray)
import Data.Char (digitToInt)
import Control.Arrow ((***))
import Data.Maybe (fromJust)

type Pos = (Int, Int)
type Chiton = UArray Pos Int

neigh :: Chiton -> Pos -> Pos -> [(Int, Pos)]
neigh g goal = map (\p -> (risk g p, p)) . filter (inRange ((0, 0), goal)) . adj4

risk :: Chiton -> Pos -> Int
risk g (y, x) = 1 + (((yinc + xinc + g ! (y `rem` height, x `rem` width)) - 1) `rem` 9)
  where ((ymin, xmin), (ymax, xmax)) = bounds g
        width = xmax - xmin + 1
        height = ymax - ymin + 1
        yinc = y `div` height
        xinc = x `div` width

compress :: Int -> Pos -> Int
compress width (y, x) = y * width + x

adj4 :: Pos -> [Pos]
adj4 (y, x) = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

dist :: Pos -> Pos -> Int
dist (y1, x1) (y2, x2) = abs (y1 - y2) + abs (x1 - x2)

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

grid :: String -> Chiton
grid s = listArray ((0, 0), (h - 1, w - 1)) (concat g)
  where g = map (map digitToInt) (lines s)
        h = length g
        w = uniform length g

main :: IO ()
main = do
  s <- readInputFile
  let g = grid s
      (start, goal) = bounds g
      width = snd goal - snd start + 1
  print (fromJust (astarInt (compress width) (neigh g goal) (dist goal) (== goal) start))
  let f x = (x + 1) * 5 - 1
  let goal2 = (f *** f) goal

  print (fromJust (astarInt (compress (5 * width)) (neigh g goal2) (dist goal2) (== goal2) start))
