import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***))
import Data.Either (partitionEithers)
import Data.List (group, sort)

points :: ((Int, Int), (Int, Int)) -> Either [(Int, Int)] [(Int, Int)]
points ((x1, y1), (x2, y2)) | x1 == x2 = Right [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
points ((x1, y1), (x2, y2)) | y1 == y2 = Right [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
points (p1, p2) = let ((xa, ya), (xb, yb)) = (min p1 p2, max p1 p2)
                      dy = if ya > yb then -1 else 1
                  in Left [(x, ya + dy * d) | (x, d) <- zip [xa .. xb] [0..]]

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

len2 :: [a] -> Bool
len2 [] = False
len2 [_] = False
len2 _ = True

coords :: String -> ((Int, Int), (Int, Int))
coords s = case words s of
  [a, "->", b] -> (pair a, pair b)
  _ -> error (s ++ " bad")

pair :: String -> (Int, Int)
pair s = (read *** read) (splitOnOne ',' s)

main :: IO ()
main = do
  s <- readInputFile
  let vents = map coords (lines s)
      (p45, p90) = partitionEithers (map points vents)
      overlaps = count len2 . group . sort . concat
  print (overlaps p90)
  print (overlaps (p90 ++ p45))
