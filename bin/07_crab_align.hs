import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Data.List (dropWhileEnd, sort)

minAlign :: [Int] -> (Int -> Int) -> [Int] -> Int
minAlign candidates cost crabs = minimum (map (\i -> alignCost i cost crabs) candidates)

alignCost :: Int -> (Int -> Int) -> [Int] -> Int
alignCost i cost = sum . map (\n -> cost (abs (n - i)))

main :: IO ()
main = do
  s <- readInputFile
  let crabs = map read (splitOn ',' (dropWhileEnd (== '\n') s))
      median = sort crabs !! (length crabs `quot` 2)
  print (alignCost median id crabs)

  let meanFloor = sum crabs `div` length crabs
  print (minAlign [meanFloor, meanFloor + 1] (\n -> n * (n + 1) `quot` 2) crabs)
