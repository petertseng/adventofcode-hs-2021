import AdventOfCode (readInputFile)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (foldl')

tryzs :: (Int -> Int -> Int) -> (Int, IntMap Int) -> (Int, Int) -> (Int, IntMap Int)
tryzs f (zmax, cands) (x, y) = (zmax', IntMap.fromListWith f cands')
  where cands' = [(z', ins * 10 + w) | (z, ins) <- IntMap.assocs cands, w <- [1 .. 9], let z' = run x y z w, z' <= zmax']
        zmax' = if x <= 0 then zmax `quot` 26 else zmax * 26 + 25

run :: Int -> Int -> Int -> Int -> Int
run xplus yplus z w = z''
  where x = (z `rem` 26) + xplus
        z' = if xplus <= 0 then z `quot` 26 else z
        z'' = if x /= w then z' * 26 + w + yplus else z'

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

prog :: [String] -> (Int, Int)
prog s = case map words [head s, s !! 4, s !! 5, s !! 15] of
  [["inp", _], ["div", "z", z], ["add", "x", x], ["add", "y", y]] -> case (read z :: Int, read x) of
    (26, xv) | xv <= 0 -> (xv, read y)
    (1, xv) | xv >= 10 -> (xv, read y)
    _ -> error ("mismatched z and x " ++ show (x, z))
  _ -> error ("bad prog " ++ show s)

main :: IO ()
main = do
  s <- readInputFile
  let chunks = chunk 18 (lines s)
      xys = map prog chunks
      cands f = foldl' (tryzs f) (0, IntMap.singleton 0 0) xys
      accepted f = snd (cands f) IntMap.! 0
  print (accepted max)
  print (accepted min)
