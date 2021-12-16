import AdventOfCode (readInputFile)

import Control.Arrow (first)
import Control.Monad (unless)
import Data.List (foldl', mapAccumL)

packet :: [Bool] -> ((Int, Int), [Bool])
packet b = ((vers + sum (map fst subs), val), b4)
  where
    (vers, b1) = num 3 b
    (t, b2) = num 3 b1
    subs :: [(Int, Int)]
    (subs, b3) = if t == 4 then ([], b2) else case bit b2 of
      (False, b5) ->
        let (tlen, b6) = num 15 b5 in first packets (splitAt tlen b6)
      (True, b5) ->
        let (tnum, b6) = num 11 b5
            f :: [Bool] -> Int -> ([Bool], (Int, Int))
            f b' _ = tflip (packet b') in
            tflip (mapAccumL f b6 [1 .. tnum])
    (val, b4) = if t == 4 then literal b2 else
      let svals = map snd subs
          binop f = case svals of
            [x, y] -> if f x y then 1 else 0
            _ -> error ("binop needs two vals not " ++ show svals)
          v = case t of
            0 -> sum svals
            1 -> product svals
            2 -> minimum svals
            3 -> maximum svals
            5 -> binop (>)
            6 -> binop (<)
            7 -> binop (==)
            _ -> error ("bad type " ++ show t)
       in (v, b3)

literal :: [Bool] -> (Int, [Bool])
literal = val 0
  where
    val acc b0 = if more then val acc' b2 else (acc', b2)
      where (more, b1) = bit b0
            (v, b2) = num 4 b1
            acc' = acc * 16 + v

packets :: [Bool] -> [(Int, Int)]
packets [] = []
packets xs = let (p, xs') = packet xs in p : packets xs'

num :: Int -> [Bool] -> (Int, [Bool])
num len bits = first (foldl' f 0) (splitAt len bits)
  where f acc b = acc * 2 + (if b then 1 else 0)

bit :: [Bool] -> (Bool, [Bool])
bit (x:xs) = (x, xs)
bit [] = error "no bit"

tflip :: (a, b) -> (b, a)
tflip (a, b) = (b, a)

ctob :: Char -> [Bool]
ctob '0' = [False, False, False, False]
ctob '1' = [False, False, False, True]
ctob '2' = [False, False, True, False]
ctob '3' = [False, False, True, True]
ctob '4' = [False, True, False, False]
ctob '5' = [False, True, False, True]
ctob '6' = [False, True, True, False]
ctob '7' = [False, True, True, True]
ctob '8' = [True, False, False, False]
ctob '9' = [True, False, False, True]
ctob 'A' = [True, False, True, False]
ctob 'B' = [True, False, True, True]
ctob 'C' = [True, True, False, False]
ctob 'D' = [True, True, False, True]
ctob 'E' = [True, True, True, False]
ctob 'F' = [True, True, True, True]
ctob '\n' = []
ctob c = error (c : " bad")

main :: IO ()
main = do
  s <- readInputFile
  let ((ver, val), unparsed) = packet (concatMap ctob s)
  print ver
  print val
  unless (all not unparsed) $ error ("unparsed " ++ show unparsed)
