{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Control.Arrow ((***))
import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (delete, foldl', maximumBy, tails)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ord (comparing)

type Pos = (Int, Int, Int)
type Rule = ((Int, Int, Int), (Int, Int, Int), Pos)

requiredOverlaps :: Int
requiredOverlaps = 12

requiredPairs :: Int
requiredPairs = sum [1 .. requiredOverlaps - 1]

overlap :: [Pos] -> [Pos] -> [Rule]
overlap s1 s2 = do
  x <- [0, 1, 2]
  (xrot, xshift) <- axisAlign 0 x s1 s2
  y <- delete x [0, 1, 2]
  (yrot, yshift) <- axisAlign 1 y s1 s2
  let z = 3 - x - y
  (zrot, zshift) <- axisAlign 2 z s1 s2
  return ((x, y, z), (xrot, yrot, zrot), (xshift, yshift, zshift))

axisAlign :: Int -> Int -> [Pos] -> [Pos] -> [(Int, Int)]
axisAlign c1 c2 s1 s2 = concatMap tryRot [1, -1]
  where
    axis1 = map (coord3 c1) s1
    axis2 = map (coord3 c2) s2
    tryRot rot = mapMaybe (tryShift rot) shifts
      where shifts = IntMap.keys (IntMap.filter (>= requiredOverlaps) (freqMap [tr1 - tr2 * rot | tr1 <- axis1, tr2 <- axis2]))
    tryShift rot shift = if ok then Just (rot, shift) else Nothing
      where
        ok = (freqMapIntersectSize `on` freqMap) axis1 axis2Tr >= requiredOverlaps
        axis2Tr = map ((+ shift) . (* rot)) axis2

 -- can't use IntSet for these: undercounts if there are equal elements on the two sides like [1, 1] and [1, 1]
freqMapIntersectSize :: IntMap Int -> IntMap Int -> Int
freqMapIntersectSize s1 = sum . IntMap.elems . IntMap.intersectionWith min s1

mightOverlap :: IntMap Int -> IntMap Int -> Bool
mightOverlap s1 = (>= requiredPairs) . freqMapIntersectSize s1

applyRules :: [(Int, Int, Rule)] -> IntMap (Set Pos) -> IntMap (Set Pos)
applyRules = flip (foldl' (flip applyRule))

applyRule :: (Int, Int, Rule) -> IntMap (Set Pos) -> IntMap (Set Pos)
applyRule (i1, i2, (p, r, t)) pts = IntMap.adjust (`Set.union` add1) i1 (IntMap.adjust (`Set.union` add2) i2 pts)
  where pts1 = pts IntMap.! i1
        pts2 = pts IntMap.! i2
        add1 = Set.map (translate t . scale r . permute p) pts2
        add2 = Set.map (permute (inversePerm p) . scale r . translate (scale (-1, -1, -1) t)) pts1

scale :: (Int, Int, Int) -> Pos -> Pos
scale = apply3 (*)

translate :: Pos -> Pos -> Pos
translate = apply3 (+)

permute :: (Int, Int, Int) -> Pos -> Pos
permute (a, b, c) p = (coord3 a p, coord3 b p, coord3 c p)

inversePerm :: (Int, Int, Int) -> (Int, Int, Int)
inversePerm p = (find3 0 p, find3 1 p, find3 2 p)

find3 :: Int -> (Int, Int, Int) -> Int
find3 a (b, _, _) | a == b = 0
find3 a (_, b, _) | a == b = 1
find3 a (_, _, b) | a == b = 2
find3 _ _ = error "not found"

pairwiseDist :: [Pos] -> [Int]
pairwiseDist = map (uncurry dist) . pairs

dist :: Pos -> Pos -> Int
dist (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

apply3 :: (a -> b -> c) -> (a, a, a) -> (b, b, b) -> (c, c, c)
apply3 f (x1, y1, z1) (x2, y2, z2) = (f x1 x2, f y1 y2, f z1 z2)

coord3 :: Int -> Pos -> Int
coord3 0 (a, _, _) = a
coord3 1 (_, b, _) = b
coord3 2 (_, _, c) = c
coord3 c (_, _, _) = error ("invalid coord coord3 " ++ show c)

freqMap :: [Int] -> IntMap Int
freqMap = IntMap.fromListWith (+) . map (, 1)

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | x:ys <- tails xs, y <- ys]

allEqual :: Eq a => [a] -> Bool
allEqual [] = error "empty allEqual"
allEqual (x:xs) = all (== x) xs

atMostOne :: [a] -> Maybe a
atMostOne [x] = Just x
atMostOne [] = Nothing
atMostOne _ = error "too many for atMostOne"

scanner :: [String] -> [Pos]
scanner ss = map point (tail ss)
  where point s = case splitOn ',' s of
          [a, b, c] -> (read a, read b, read c)
          _ -> error ("bad point " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let scanners = map scanner (splitOn "" (lines s))
      dists = map (freqMap . pairwiseDist) scanners
      trd3 (_, _, a) = a
      fstsnd3 (a, b, _) = (a, b)
      candidatePairs = map (fstsnd3 *** fstsnd3) (filter (uncurry (mightOverlap `on` trd3)) (pairs (zip3 [0..] scanners dists)))
      rules = mapMaybe (\((i1, s1), (i2, s2)) -> fmap (i1, i2,) (atMostOne (overlap s1 s2))) candidatePairs
      applyRulesUntil f to = until (f . map Set.size . IntMap.elems) (applyRules rules) (IntMap.fromAscList (zip [0..] to))
      points = applyRulesUntil allEqual (map Set.fromList scanners)
  print (Set.size (points IntMap.! 0))

  let scansScans = applyRulesUntil (elem (length scanners)) (replicate (length scanners) (Set.singleton (0, 0, 0)))
      scans = maximumBy (comparing Set.size) (IntMap.elems scansScans)
  print (maximum (map (uncurry dist) (pairs (Set.toList scans))))
