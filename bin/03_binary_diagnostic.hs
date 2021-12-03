import AdventOfCode (readInputFile)

import Data.Bits (shiftL, testBit)
import Data.Function (on)
import Data.List (foldl', partition)

majorityBits :: Int -> [Int] -> Int
majorityBits len xs = sum [bit i `shiftL` i | i <- [0 .. len - 1]]
  where bit i = case uncurry (compare `on` length) (partition (`testBit` i) xs) of
                  LT -> 0
                  GT -> 1
                  EQ -> error "equal"

filterByFreq :: Bool -> Int -> [Int] -> Int
filterByFreq _ _ [x] = x
filterByFreq _ _ [] = error "no candidates"
filterByFreq more i xs = filterByFreq more (i - 1) keep
  where (os, zs) = partition (`testBit` i) xs
        keep = if (length os < length zs) /= more then os else zs

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

binToInt :: String -> Int
binToInt = foldl' shadd 0
  where shadd n '0' = n * 2
        shadd n '1' = n * 2 + 1
        shadd _ c = error (c : " bad binToInt")

main :: IO ()
main = do
  s <- readInputFile
  let strs = lines s
      len = uniform length strs
      nums = map binToInt strs
      a = majorityBits len nums
      b = (1 `shiftL` len) - 1 - a
  print (a * b)

  let filterByFreq' most = filterByFreq most (len - 1) nums
  print (filterByFreq' True * filterByFreq' False)
