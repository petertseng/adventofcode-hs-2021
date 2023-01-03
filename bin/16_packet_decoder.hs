import AdventOfCode (readInputFile)

import Control.Arrow (first)
import Control.Monad (replicateM, unless)
import Data.List (foldl')

newtype Parser a = Parser {parse :: [Bool] -> (a, [Bool])}

instance Functor Parser where
  fmap f (Parser p) = Parser (first f . p)

instance Applicative Parser where
  pure x = Parser (\y -> (x, y))
  Parser l <*> Parser r = Parser (\x -> let (l', rest) = l x in first l' (r rest))

instance Monad Parser where
  return = pure
  Parser l >>= r = Parser (\x -> let (l', rest) = l x in parse (r l') rest)

packet :: Parser (Int, Int)
packet = do
  vers <- num 3
  t <- num 3
  subs <- if t == 4 then return [] else do
    lengthTypeNum <- bit
    if lengthTypeNum then do
      tnum <- num 11
      replicateM tnum packet
    else do
      tlen <- num 15
      packets tlen
  val <- if t == 4 then literal else do
    let svals = map snd subs
        binop f = case svals of
          [x, y] -> if f x y then 1 else 0
          _ -> error ("binop needs two vals not " ++ show svals)
        v = case t of
          0 -> sum svals
          1 -> product svals
          2 -> minimum svals
          3 -> maximum svals
          5 -> binop (>)
          6 -> binop (<)
          7 -> binop (==)
          _ -> error ("bad type " ++ show t)
    return v
  return (vers + sum (map fst subs), val)

literal :: Parser Int
literal = val 0
  where val acc = do
          more <- bit
          v <- num 4
          (if more then val else return) (acc * 16 + v)

packets :: Int -> Parser [(Int, Int)]
packets bitlen = Parser (first packets' . splitAt bitlen)
  where packets' :: [Bool] -> [(Int, Int)]
        packets' [] = []
        packets' xs = let (a, xs') = parse packet xs in a : packets' xs'

num :: Int -> Parser Int
num len = Parser (first (foldl' f 0) . splitAt len)
  where f acc b = acc * 2 + (if b then 1 else 0)

bit :: Parser Bool
bit = Parser bit'
  where bit' (x:xs) = (x, xs)
        bit' [] = error "no bit"

ctob :: Char -> [Bool]
ctob '0' = [False, False, False, False]
ctob '1' = [False, False, False, True]
ctob '2' = [False, False, True, False]
ctob '3' = [False, False, True, True]
ctob '4' = [False, True, False, False]
ctob '5' = [False, True, False, True]
ctob '6' = [False, True, True, False]
ctob '7' = [False, True, True, True]
ctob '8' = [True, False, False, False]
ctob '9' = [True, False, False, True]
ctob 'A' = [True, False, True, False]
ctob 'B' = [True, False, True, True]
ctob 'C' = [True, True, False, False]
ctob 'D' = [True, True, False, True]
ctob 'E' = [True, True, True, False]
ctob 'F' = [True, True, True, True]
ctob '\n' = []
ctob c = error (c : " bad")

main :: IO ()
main = do
  s <- readInputFile
  let ((ver, val), unparsed) = parse packet (concatMap ctob s)
  print ver
  print val
  unless (all not unparsed) $ error ("unparsed " ++ show unparsed)
