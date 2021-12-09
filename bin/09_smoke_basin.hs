import AdventOfCode (readInputFile)
import AdventOfCode.Search (bfs)

import Data.Array.Unboxed ((!), (!?), UArray, assocs, listArray)
import Data.Char (digitToInt)
import Data.Either (rights)
import Data.List (foldl', sortBy)
import Data.Maybe (mapMaybe)

type Heights = UArray (Int, Int) Int

lows :: Heights -> [((Int, Int), Int)]
lows hgts = filter low (assocs hgts)
  where low ((y, x), v) = all (ok v) [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]
        ok h npos = maybe True (h <) (hgts !? npos)

basin :: Heights -> (Int, Int) -> Int
basin hgts pos | hgts ! pos == 9 = 0
basin hgts pos = length (rights (bfs (neighs hgts) (const True) pos))

neighs :: Heights -> (Int, Int) -> [(Int, Int)]
neighs hgts pos = mapMaybe (neigh hgts pos) [(-1, 0), (0, -1), (0, 1), (1, 0)]

neigh :: Heights -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
neigh hgts (y, x) (dy, dx) = hgts !? npos >>= \v -> if v == 9 then Nothing else Just npos
  where npos = (y + dy, x + dx)

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

grid :: String -> Heights
grid s = listArray ((1, 1), (h, w)) (concat g)
  where g = map (map digitToInt) (lines s)
        h = length g
        w = uniform length g

main :: IO ()
main = do
  s <- readInputFile
  let g = grid s
      lows' = lows g
  print (sum (map (succ . snd) lows'))

  let basins = map (basin g . fst) lows'
      biggest = take 3 (sortBy (flip compare) basins)
  print (foldl' (*) (1 :: Int) biggest)
