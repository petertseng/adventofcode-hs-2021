import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Control.Arrow (first, second)
import Data.Bits (shiftL, testBit)
import Data.List (dropWhileEnd, foldl')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (mapMaybe)

type Interval = (Int, Int)

tsOfX :: Int -> Int -> Int -> Int -> Maybe Interval
tsOfX xmin xmax tmax vx0 = step 0 0 vx0 Nothing
  where step _ x _ ts | x > xmax = ts
        step _ x 0 _ | x < xmin = Nothing
        step t x vx _ | x < xmin = step (t + 1) (x + vx) (vx - 1) Nothing
        step t _ 0 ts = maybe (Just (t, t)) (\(tmin, _) -> Just (tmin, tmax)) ts
        step t x vx ts = step (t + 1) (x + vx) (vx - 1) (maybe (Just (t, t)) (\(tmin, _) -> Just (tmin, t)) ts)

tsOfY :: Int -> Int -> Int -> Maybe Interval
tsOfY ymin ymax = interval . map t . takeWhile inRange . dropWhile tooHigh . iterate step . initial
  where initial vy0 | vy0 > 0 = (2 * vy0 + 1, 0, -vy0 - 1)
        initial vy0 = (0, 0, vy0)
        step (t', p, v) = (t' + 1, p + v, v - 1)
        tooHigh (_, p, _) = p > ymax
        inRange (_, p, _) = p >= ymin
        t (a, _, _) = a

addEvent :: (Int, (Int, Int)) -> (Int, Int) -> (Int, (Int, Int))
addEvent (s, active) (event, freq) | event `testBit` 1 = (s, update (subtract freq) active)
  where update = if even event then first else second
addEvent (s, active) (event, freq) = (s + freq * otherFreq active, update (+ freq) active)
  where update = if even event then first else second
        otherFreq = if even event then snd else fst

intervalEvents :: Int -> [Interval] -> IntMap Int
intervalEvents bit = IntMap.fromListWith (+) . concatMap event
  where event (a, b) = [((a `shiftL` 2) + bit, 1), ((b `shiftL` 2) + 2 + bit, 1)]

interval :: [Int] -> Maybe Interval
interval [] = Nothing
interval [x] = Just (x, x)
interval (x:xs) = Just (x, last xs)

target :: String -> ((Int, Int), (Int, Int))
target s = case words s of
  ["target", "area:", 'x':'=':x, 'y':'=':y] -> (range (dropWhileEnd (== ',') x), range y)
  _ -> error ("bad target " ++ s)
  where range s' = case splitOn '.' s' of
          [mn, mx] -> (read mn, read mx)
          x -> error ("bad range " ++ show x ++ " in " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let ((xmin, xmax), (ymin, ymax)) = target s
  -- TODO: Can only use this solution under conditions:
  -- xmin..xmax contains a triangular number
  -- Y has time to grow
  -- Check both conditions.
  print (ymin * (ymin + 1) `div` 2)
  let ys = intervalEvents 0 (mapMaybe (tsOfY ymin ymax) [ymin .. -ymin - 1])
      xs = intervalEvents 1 (mapMaybe (tsOfX xmin xmax (-ymin * 2)) [1 .. xmax])
      events = IntMap.unionWith (error "should not clash") xs ys
      (intersects, _) = foldl' addEvent (0, (0, 0)) (IntMap.assocs events)
  print intersects
