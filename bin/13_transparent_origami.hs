import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***), first, second)
import Data.Foldable (for_)
import Data.Set (Set)
import qualified Data.Set as Set

type Point = (Int, Int)

foldAlong :: Set Point -> (Point -> Point) -> Set Point
foldAlong = flip Set.map

fold :: String -> Point -> Point
fold s = let fold' along v = if v <= along then v else along * 2 - v in
  case words s of
  ["fold", "along", 'x':'=':x] -> first (fold' (read x))
  ["fold", "along", 'y':'=':y] -> second (fold' (read y))
  _ -> error ("bad fold " ++ s)

point :: String -> Point
point = (read *** read) . splitOnOne ','

main :: IO ()
main = do
  s <- readInputFile
  let (points, folds) = splitOnOne "" (lines s)
      foldedPoints = scanl foldAlong (Set.fromList (map point points)) (map fold folds)
  print (length (foldedPoints !! 1))

  let points' = last foldedPoints
      (xs, ys) = unzip (Set.toList points')
      yr = [minimum ys .. maximum ys]
      xr = [minimum xs .. maximum xs]
  for_ yr (\y -> putStrLn (map (\x -> if (x, y) `Set.member` points' then '#' else ' ') xr))
