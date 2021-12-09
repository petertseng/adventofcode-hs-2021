import AdventOfCode (readInputFile)
import AdventOfCode.Search (bfs)

import Data.Array.Unboxed ((!), UArray, assocs, bounds, listArray)
import Data.Char (digitToInt)
import Data.Either (rights)
import Data.List (foldl', sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(Down), comparing)

type Heights = UArray (Int, Int) Int

lows :: Heights -> [((Int, Int), Int)]
lows hgts = filter low (assocs hgts)
  where (_, (h, w)) = bounds hgts
        low ((y, x), v) | y > 1 && v >= hgts ! (y - 1, x) = False
        low ((y, x), v) | x > 1 && v >= hgts ! (y, x - 1) = False
        low ((y, x), v) | y < h && v >= hgts ! (y + 1, x) = False
        low ((y, x), v) | x < w && v >= hgts ! (y, x + 1) = False
        low _ = True

basin :: Heights -> (Int, Int) -> Int
basin hgts pos | hgts ! pos == 9 = 0
basin hgts pos = length (rights (bfs (neighs hgts) (const True) pos))

neighs :: Heights -> (Int, Int) -> [(Int, Int)]
neighs hgts pos = mapMaybe (neigh hgts pos) [(-1, 0), (0, -1), (0, 1), (1, 0)]

neigh :: Heights -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
neigh hgts (y, x) (dy, dx) | inBounds npos = let v = hgts ! npos in if v == 9 then Nothing else Just npos
  where (_, (h, w)) = bounds hgts
        npos = (y + dy, x + dx)
        inBounds (yy, xx) = yy >= 1 && xx >= 1 && yy <= h && xx <= w
neigh _ _ _ = Nothing

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
      -- https://ro-che.info/articles/2016-04-02-descending-sort-haskell
      {- HLINT ignore: "Use sortOn" -}
      biggest = take 3 (sortBy (comparing Down) basins)
  print (foldl' (*) (1 :: Int) biggest)
