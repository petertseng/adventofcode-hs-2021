import AdventOfCode (readInputFile)

import Control.Arrow ((&&&))
import Data.Char (digitToInt, isDigit)
import Data.List (unfoldr)

data Snailfish = Reg Int | Pair Snailfish Snailfish deriving (Eq, Show)

reduce :: Snailfish -> Snailfish
reduce sf = last (sf : unfoldr (fmap (id &&& id) . explodeOrSplit) sf)

explodeOrSplit :: Snailfish -> Maybe Snailfish
explodeOrSplit sf = case explode 0 sf of
  Just (_, _, sf') -> Just sf'
  Nothing -> split sf

explode :: Int -> Snailfish -> Maybe (Int, Int, Snailfish)
explode _ (Reg _) = Nothing
explode 4 (Pair (Reg l) (Reg r)) = Just (l, r, Reg 0)
explode lvl sf | lvl >= 4 = error ("bad explode " ++ show (lvl, sf))
explode lvl (Pair l r) = case explode (lvl + 1) l of
  Just (ladd, radd, l') -> Just (ladd, 0, Pair l' (addLeft radd r))
  Nothing -> fmap (\(ladd, radd, r') -> (0, radd, Pair (addRight ladd l) r')) (explode (lvl + 1) r)

addLeft :: Int -> Snailfish -> Snailfish
addLeft 0 sf = sf
addLeft a (Reg n) = Reg (n + a)
addLeft a (Pair l r) = Pair (addLeft a l) r

addRight :: Int -> Snailfish -> Snailfish
addRight 0 sf = sf
addRight a (Reg n) = Reg (n + a)
addRight a (Pair l r) = Pair l (addRight a r)

split :: Snailfish -> Maybe Snailfish
split (Reg n) | n < 10 = Nothing
split (Reg n) = Just (Pair (Reg (n `quot` 2)) (Reg (n - (n `quot` 2))))
split (Pair l r) = case split l of
  Just l' -> Just (Pair l' r)
  Nothing -> fmap (Pair l) (split r)

magnitude :: Snailfish -> Int
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r
magnitude (Reg n) = n

snailfish :: String -> Snailfish
snailfish s0 = case snailfish' s0 of
  (sf, "") -> sf
  (sf, s) -> error ("unparsed " ++ s ++ " after " ++ show sf)
  where
    snailfish' :: String -> (Snailfish, String)
    snailfish' (d:s) | isDigit d = (Reg (digitToInt d), s)
    snailfish' (',':s) = snailfish' s
    snailfish' ('[':s) =
      let (l, s2) = snailfish' s
          (r, s3) = snailfish' s2
      in case s3 of
        (']':s4) -> (Pair l r, s4)
        _ -> error ("expected ] to close snailfish not " ++ s3)
    snailfish' s = error ("bad smallfish " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let hw = map snailfish (lines s)
  print (magnitude (foldl1 (\a b -> reduce (Pair a b)) hw))
  print (maximum [magnitude (reduce (Pair a b)) | a <- hw, b <- hw, a /= b])
