import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((&&&))
import Data.Array.Unboxed ((!), UArray, accumArray, array, assocs)
import Data.Char (ord)
import Data.List ((\\), foldl')

easy :: String -> Bool
easy d = l == 2 || l == 3 || l == 4 || l == 7
  where l = length d

unscramble :: ([String], [String]) -> Int
unscramble (samples, outs) = foldl' (\acc x -> acc * 10 + digit' x) 0 outs
  where code1 = one (filter ((== 2) . length) samples)
        code4 = one (filter ((== 4) . length) samples)
        code7 = one (filter ((== 3) . length) samples)
        letterIndex x = ord x - ord 'a'
        a = letterIndex (one (code7 \\ code1))
        identByFreq (c, 8) | c == a = 0x1 --a
        identByFreq (_, 6) = 0x2 --b
        identByFreq (_, 8) = 0x4 --c
        identByFreq (c, 7) | c `elem` map letterIndex code4 = 0x8 --d
        identByFreq (_, 4) = 0x10 -- e
        identByFreq (_, 9) = 0x20 -- f
        identByFreq (_, 7) = 0x40 -- g
        identByFreq x = error ("bad freq " ++ show x)
        tally :: UArray Int Int
        tally = accumArray (+) 0 (0, 6) [(letterIndex c, 1) | c <- concat samples]
        seg :: UArray Int Int
        seg = array (0, 6) (map (fst &&& identByFreq) (assocs tally))
        digit' = digit . sum . map ((seg !) . letterIndex)

digit :: Int -> Int
digit 0x77 = 0
digit 0x24 = 1
digit 0x5d = 2
digit 0x6d = 3
digit 0x2e = 4
digit 0x6b = 5
digit 0x7b = 6
digit 0x25 = 7
digit 0x7f = 8
digit 0x6f = 9
digit x = error (show x ++ " bad digit")

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

one :: [a] -> a
one [x] = x
one [] = error "no one"
one _ = error "too many one"

main :: IO ()
main = do
  s <- readInputFile
  let disps = map (splitOnOne "|" . words) (lines s)
  print (sum (map (count easy . snd) disps))
  print (sum (map unscramble disps))
