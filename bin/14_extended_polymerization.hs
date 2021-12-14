{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

type Counts = (Map (Char, Char) Int, Map Char Int)

step :: Map (Char, Char) Char -> Counts -> Counts
step rules (doubles, singles) = (Map.fromListWith (+) (concat doubles'), Map.unionWith (+) singles (Map.fromListWith (+) (catMaybes singles')))
  where (doubles', singles') = unzip (map expand (Map.assocs doubles))
        expand (p@(l, r), v) = case Map.lookup p rules of
          Nothing -> ([(p, v)], Nothing)
          Just c -> ([((l, c), v), ((c, r), v)], Just (c, v))

freqDiff :: (a, Map b Int) -> Int
freqDiff (_, singles) = maximum vs - minimum vs
  where vs = Map.elems singles

freqMap :: Ord a => [a] -> Map a Int
freqMap = Map.fromListWith (+) . map (, 1)

rule :: String -> ((Char, Char), Char)
rule s = case words s of
  [a:[b], "->", [c]] -> ((a, b), c)
  _ -> error ("bad rule " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let (poly, rules) = case lines s of
        [_, _] -> error "no rules"
        [_] -> error "no rules"
        [] -> error "no poly"
        (p:"":rs) -> (p, Map.fromListWith (error "duplicate") (map rule rs))
        (_:(_:_):_) -> error "second line should be empty"
  let doubles = freqMap (zip poly (drop 1 poly))
      singles = freqMap poly
      steps = iterate (step rules) (doubles, singles)
  print (freqDiff (steps !! 10))
  print (freqDiff (steps !! 40))
