import AdventOfCode (readInputFile)

import Data.Either (partitionEithers)
import Data.List (foldl', sort)

score :: String -> Either Int Int
score = parse ""
  where parse opens "" = Right (complete opens)
        parse ('(':opens) (')':s') = parse opens s'
        parse ('[':opens) (']':s') = parse opens s'
        parse ('{':opens) ('}':s') = parse opens s'
        parse ('<':opens) ('>':s') = parse opens s'
        parse _ (')':_) = Left 3
        parse _ (']':_) = Left 57
        parse _ ('}':_) = Left 1197
        parse _ ('>':_) = Left 25137
        parse opens (c:s') | compScore c /= 0 = parse (c:opens) s'
        -- this will actually never be reached because compScore will error.
        parse _ (c:_) = error (c : " bad score")

median :: [Int] -> Int
median xs = sort xs !! (length xs `quot` 2)

complete :: String -> Int
complete = foldl' add 0
  where add acc c = 5 * acc + compScore c

compScore :: Char -> Int
compScore '(' = 1
compScore '[' = 2
compScore '{' = 3
compScore '<' = 4
compScore c = error (c : " bad complete")

main :: IO ()
main = do
  s <- readInputFile
  let (corrupt, incomplete) = partitionEithers (map score (lines s))
  print (sum corrupt)
  print (median incomplete)
