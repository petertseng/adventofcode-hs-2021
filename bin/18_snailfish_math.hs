import AdventOfCode (readInputFile)

import Control.Arrow (first, second)
import Data.Char (digitToInt, isDigit)
import Data.List (foldl')

type Snailfish = [(Int, Int)]

add :: Snailfish -> Snailfish -> Snailfish
add a b = map (first succ) (a ++ b)

reduce :: Snailfish -> Snailfish
reduce = split . explode

explode :: Snailfish -> Snailfish
explode = drop 1 . explode' (-1, -1)

explode' :: (Int, Int) -> Snailfish -> Snailfish
explode' (d, n) [] = [(d, n)]
explode' _ ((d, _):_) | d > 5 = error "bad depth"
explode' _ [(d, _)] | d == 5 = error "can't explode single"
explode' _ ((d1, _):(d2, _):_) | d1 == 5 && d1 /= d2 = error "inconsistent depth"
explode' (pd, pn) ((d1, n1):(_, n2):xs) | d1 >= 5 = (pd, pn + n1) : explode' (d1 - 1, 0) (headAdd n2 xs)
explode' prev ((d, n):xs) = prev : explode' (d, n) xs

split :: Snailfish -> Snailfish
split = split' []

split' :: Snailfish -> Snailfish -> Snailfish
split' lefts [] = reverse lefts
split' _ ((d, _):_) | d > 4 = error "bad depth"
split' [] ((d, n):xs) | d == 4 && n >= 10 = split' [(d, 0)] (headAdd ((n + 1) `quot` 2) xs)
split' ((ld, ln):lefts) ((d, n):xs) | d == 4 && n >= 10 = split' lefts ((ld, ln + (n `quot` 2)) : (d, 0) : headAdd ((n + 1) `quot` 2) xs)
split' lefts ((d, n):xs) | n >= 10 = split' lefts ((d + 1, n `quot` 2) : (d + 1, (n + 1) `quot` 2) : xs)
split' lefts (x:xs) = split' (x : lefts) xs

magnitude :: Snailfish -> Int
magnitude = magnitude' []

magnitude' :: Snailfish -> Snailfish -> Int
magnitude' [] [] = 0
magnitude' [(0, n)] [] = n
magnitude' lefts [] = error ("bad final magnitude " ++ show lefts)
magnitude' ((ld, ln):lefts) ((d, n):xs) | ld == d = magnitude' lefts ((d - 1, ln * 3 + n * 2) : xs)
magnitude' lefts (x:xs) = magnitude' (x : lefts) xs

headAdd :: Int -> Snailfish -> Snailfish
headAdd n = headMap (second (+ n))

headMap :: (a -> a) -> [a] -> [a]
headMap _ [] = []
headMap f (x:xs) = f x : xs

snailfish :: String -> Snailfish
snailfish = reverse . fst . foldl' sn ([], 0)
  where sn (nums, d) '[' = (nums, d + 1)
        sn (nums, d) ',' = (nums, d)
        sn (nums, d) c | isDigit c = ((d, digitToInt c) : nums, d)
        sn (nums, d) ']' = (nums, d - 1)
        sn _ c = error (c : " bad")

main :: IO ()
main = do
  s <- readInputFile
  let hw = map snailfish (lines s)
  print (magnitude (foldl1 (\a b -> reduce (add a b)) hw))
  print (maximum [magnitude (reduce (add a b)) | a <- hw, b <- hw, a /= b])
