import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Control.Monad (unless)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (foldl')

winTime :: IntMap Int -> [[Int]] -> (Int, Int)
winTime callTime nums = (winTime', sum (filter ((> winTime') . callTimeOrMax) (concat nums)))
  where height = length nums
        width = uniform length nums
        winTime' = minimum rowColWinTimes
        callTimeOrMax n = IntMap.findWithDefault (IntMap.size callTime) n callTime
        rowColWinTimes = foldl' delayWin (IntMap.fromList [(i, 0) | i <- [0 .. (height + width - 1)]]) (enumGrid nums)
        delayWin m (n, (y, x)) = let t = callTimeOrMax n in
                                 IntMap.adjust (max t) y (IntMap.adjust (max t) (x + height) m)

one :: [a] -> a
one [x] = x
one [] = error "empty for one"
one _ = error "too many for one"

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

enumGrid :: [[a]] -> [(a, (Int, Int))]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x cell -> (cell, (y, x))) [0..]

main :: IO ()
main = do
  s <- readInputFile
  let (order, boards) = case splitOn "" (lines s) of
        [o]:b -> (map read (splitOn ',' o), map (map (map read . words)) b)
        _ -> error ("bad boards " ++ s)
      callTime = IntMap.fromList (zip order [0..])
  unless (IntMap.size callTime == length order) $ error "duplicate"

  let winTimes = map (winTime callTime) boards
      printWin (t, sm) = print ((order !! t) * sm)
      printWinBy f = let t = f (map fst winTimes) in
                     printWin (one (filter ((== t) . fst) winTimes))

  printWinBy minimum
  printWinBy maximum
