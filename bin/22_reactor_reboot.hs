import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

type Interval = (Int, Int)
type Box = (Interval, Interval, Interval)
data Command = On | Off deriving (Eq, Show)

-- according to the commands, how many cells are lit within the specified box?
-- first command that matches wins.
countOn :: [(Command, Box)] -> Box -> Int
countOn _ b | boxEmpty b = 0
countOn [] _ = 0
countOn ((cmd, b) : xs) b2 = let inter = boxInter b b2 in
  if boxEmpty inter then countOn xs b2
  else (if cmd == Off then 0 else boxSize inter) + sum (map (countOn xs) (b2 `boxSub` inter))

boxSub :: Box -> Box -> [Box]
((x1, x2), y@(y1, y2), z@(z1, z2)) `boxSub` (xi@(xi1, xi2), yi@(yi1, yi2), (zi1, zi2)) =
  [ ((x1, xi1 - 1), y, z)
  , ((xi2 + 1, x2), y, z)
  , (xi, (y1, yi1 - 1), z)
  , (xi, (yi2 + 1, y2), z)
  , (xi, yi, (z1, zi1 - 1))
  , (xi, yi, (zi2 + 1, z2))
  ]

boxInter :: Box -> Box -> Box
boxInter (x1, y1, z1) (x2, y2, z2) = (interInter x1 x2, interInter y1 y2, interInter z1 z2)

boxSize :: Box -> Int
boxSize (x, y, z) = interSize x * interSize y * interSize z

boxEmpty :: Box -> Bool
boxEmpty (x, y, z) = interEmpty x || interEmpty y || interEmpty z

interInter :: Interval -> Interval -> Interval
interInter (a, b) (c, d) = (max a c, min b d)

interSize :: Interval -> Int
interSize (a, b) = b - a + 1

interEmpty :: Interval -> Bool
interEmpty (a, b) = a > b

minmax :: [Interval] -> Interval
minmax xs = (minimum ls, maximum rs)
  where (ls, rs) = unzip xs

boxinst :: String -> (Command, Box)
boxinst s = case words s of
  ["on", b] -> (On, box b)
  ["off", b] -> (Off, box b)
  _ -> error ("bad inst " ++ s)

box :: String -> Box
box s = case splitOn ',' s of
  ['x':'=':x, 'y':'=':y, 'z':'=':z] -> (range x, range y, range z)
  _ -> error ("bad box " ++ s)
  where range s' = case splitOn '.' s' of
          [mn, mx] -> (read mn, read mx)
          x -> error ("bad range " ++ show x ++ " in " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let insts = map boxinst (lines s)
      boxes = map snd insts
      r50 = (-50, 50)
      rev = reverse insts
  print (countOn rev (r50, r50, r50))

  let (xs, ys, zs) = unzip3 boxes
  print (countOn rev (minmax xs, minmax ys, minmax zs))
