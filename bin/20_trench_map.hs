import AdventOfCode (readInputFile)

import Data.Array.Unboxed ((!), UArray, listArray)
import Data.Bits ((.|.), (.&.), shiftL)
import Data.List (unfoldr, zip4)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

step :: UArray Int Bool -> (Set Pos, Pos, Pos, Bool) -> (Int, (Set Pos, Pos, Pos, Bool))
step rule (pts, (minY, minX), (maxY, maxX), def) = (Set.size pts', (pts', (minY - 1, minX - 1), (maxY + 1, maxX + 1), rule ! if def then 511 else 0))
  where pts' = Set.fromAscList (concatMap writeRow [minY - 1 .. maxY + 1])
        writeRow y = mapMaybe enhanced (zip4 [minX - 1 .. maxX + 1] (scanRow (y - 1)) (scanRow y) (scanRow (y + 1)))
          where enhanced (x, s1, s2, s3) = if rule ! (s1 `shiftL` 6 .|. s2 `shiftL` 3 .|. s3) then Just (y, x) else Nothing
        scanRow y | y < minY || y > maxY = replicate (maxX - minX + 3) (if def then 7 else 0)
        scanRow y = drop 1 (scanl shiftReadX (if def then 3 else 0) [minX - 1 .. maxX + 1])
          where shiftReadX a x = ((a `shiftL` 1) .&. 7) .|. val (y, x + 1)
        val p = if (if active p then p `Set.member` pts else def) then 1 else 0
        active (y, x) = minY <= y && y <= maxY && minX <= x && x <= maxX

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

enumGrid :: [[a]] -> [(Pos, a)]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [0..]

lit :: Char -> Bool
lit '#' = True
lit '.' = False
lit c = error (c : " not lit")

main :: IO ()
main = do
  s <- readInputFile
  let (enh, grid) = case lines s of
        [_, _] -> error "no grid"
        [_] -> error "no grid"
        [] -> error "no enhance"
        (e:"":g) -> (listArray (0, 511) (map lit e), g)
        (_:(_:_):_) -> error "second line should be empty"
      posIfLit (p, b) = if b then Just p else Nothing
      points = Set.fromAscList (mapMaybe posIfLit (enumGrid (map (map lit) grid)))
      height = length grid
      width = uniform length grid
      counts = unfoldr (Just . step enh) (points, (0, 0), (height - 1, width - 1), False)
  print (counts !! 1)
  print (counts !! 49)
