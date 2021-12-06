import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Data.List (dropWhileEnd, foldl')

type Fish = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

sim :: Fish -> Fish
sim (x0, x1, x2, x3, x4, x5, x6, x7, x8) = (x1, x2, x3, x4, x5, x6, x7 + x0, x8, x0)

sumFish :: Fish -> Int
sumFish (x0, x1, x2, x3, x4, x5, x6, x7, x8) = x0 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8

addFish :: Fish -> String -> Fish
addFish (x0, x1, x2, x3, x4, x5, x6, x7, x8) "0" = (x0 + 1, x1, x2, x3, x4, x5, x6, x7, x8)
addFish (x0, x1, x2, x3, x4, x5, x6, x7, x8) "1" = (x0, x1 + 1, x2, x3, x4, x5, x6, x7, x8)
addFish (x0, x1, x2, x3, x4, x5, x6, x7, x8) "2" = (x0, x1, x2 + 1, x3, x4, x5, x6, x7, x8)
addFish (x0, x1, x2, x3, x4, x5, x6, x7, x8) "3" = (x0, x1, x2, x3 + 1, x4, x5, x6, x7, x8)
addFish (x0, x1, x2, x3, x4, x5, x6, x7, x8) "4" = (x0, x1, x2, x3, x4 + 1, x5, x6, x7, x8)
addFish (x0, x1, x2, x3, x4, x5, x6, x7, x8) "5" = (x0, x1, x2, x3, x4, x5 + 1, x6, x7, x8)
addFish (x0, x1, x2, x3, x4, x5, x6, x7, x8) "6" = (x0, x1, x2, x3, x4, x5, x6 + 1, x7, x8)
addFish (x0, x1, x2, x3, x4, x5, x6, x7, x8) "7" = (x0, x1, x2, x3, x4, x5, x6, x7 + 1, x8)
addFish (x0, x1, x2, x3, x4, x5, x6, x7, x8) "8" = (x0, x1, x2, x3, x4, x5, x6, x7, x8 + 1)
addFish _ s = error (s ++ " bad fish")

main :: IO ()
main = do
  s <- readInputFile
  let fish = foldl' addFish (0, 0, 0, 0, 0, 0, 0, 0, 0) (splitOn ',' (dropWhileEnd (== '\n') s))
  print (sumFish (iterate sim fish !! 80))
  print (sumFish (iterate sim fish !! 256))
