import AdventOfCode (readInputFile)

countIncreasingPairs :: Int -> [Int] -> Int
countIncreasingPairs n xs = count (uncurry (<)) pairs
  where pairs = zip xs (drop n xs)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let measures = map read (lines s)
  print (countIncreasingPairs 1 measures)
  print (countIncreasingPairs 3 measures)
