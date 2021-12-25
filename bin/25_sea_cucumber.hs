import AdventOfCode (readInputFile)

import Data.Bits ((.|.), (.&.), complement, shiftL, shiftR)
import Data.List (foldl', unfoldr)

type Herds = (Integer, Integer)

step :: Int -> Int -> Herds -> Maybe ((), Herds)
step height width (east, south) = if moving then Just ((), (east', south')) else Nothing
  where size = height * width

        eachRow = foldl' (\a _ -> a `shiftL` width .|. 1) 0 (replicate height ())
        eachCol = (1 `shiftL` width) - 1

        leftCol = (1 `shiftL` (width - 1)) * eachRow
        rightCol = 1 * eachRow
        topRow = eachCol `shiftL` (size - width)
        bottomRow = 1 * eachCol

        shleft bits = (bits .&. complement leftCol) `shiftL` 1 .|. (bits .&. leftCol) `shiftR` (width - 1)
        shright bits = (bits .&. complement rightCol) `shiftR` 1 .|. (bits .&. rightCol) `shiftL` (width - 1)
        shup bits = (bits .&. complement topRow) `shiftL` width .|. (bits .&. topRow) `shiftR` (size - width)
        shdown bits = (bits .&. complement bottomRow) `shiftR` width .|. (bits .&. bottomRow) `shiftL` (size - width)

        movingEast = east .&. complement (shleft (east .|. south))
        east' = east .&. complement movingEast .|. shright movingEast
        movingSouth = south .&. complement (shup (east' .|. south))
        south' = south .&. complement movingSouth .|. shdown movingSouth
        moving = movingEast /= 0 || movingSouth /= 0

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

herd :: (Integer, Integer) -> Char -> (Integer, Integer)
herd (east, south) '>' = (east `shiftL` 1 .|. 1, south `shiftL` 1)
herd (east, south) 'v' = (east `shiftL` 1, south `shiftL` 1 .|. 1)
herd (east, south) '.' = (east `shiftL` 1, south `shiftL` 1)
herd (_, _) c = error (c : " bad")

main :: IO ()
main = do
  s <- readInputFile
  let grid = lines s
      height = length grid
      width = uniform length grid
      herds = foldl' herd (0, 0) (concat grid)
  print (1 + length (unfoldr (step height width) herds))
