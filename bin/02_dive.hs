import AdventOfCode (readInputFile)

import Data.List (foldl')

data Dir = Forward | Up | Down

move :: (Int, Int, Int) -> (Dir, Int) -> (Int, Int, Int)
move (x, y, a) (Forward, n) = (x + n, y + n * a, a)
move (x, y, a) (Up, n) = (x, y, a - n)
move (x, y, a) (Down, n) = (x, y, a + n)

cmd :: String -> (Dir, Int)
cmd s = case words s of
  [d, i] -> (dir d, read i)
  _ -> error ("bad " ++ s)

dir :: String -> Dir
dir "forward" = Forward
dir "up" = Up
dir "down" = Down
dir s = error ("bad " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let cmds = map cmd (lines s)
      (x, y, a) = foldl' move (0, 0, 0) cmds
  print (x * a)
  print (x * y)
