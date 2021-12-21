import AdventOfCode (readInputFile)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (foldl', unfoldr)
import Data.Either (partitionEithers)

-- score, pos
type Player = (Int, Int)
type Game = (Player, Player)

gameDet :: Int -> Int -> Int
gameDet pos1 pos2 = scoreTimesDice (until winner stepDet (0, ((0, pos1 - 1), (0, pos2 - 1))))
  where winner (_, (_, (b, _))) = b >= 1000
        scoreTimesDice (t, ((a, _), _)) = a * t * 3

stepDet :: (Int, Game) -> (Int, Game)
stepDet (t, (p1, p2)) = (t + 1, (p2, advance roll p1))
  where roll = ((9 * t + 3) `rem` 100) + 3

advance :: Int -> Player -> Player
advance roll (score, pos) = (score + pos' + 1, pos')
  where pos' = (pos + roll) `rem` 10

gameMulti :: Int -> Int -> Int
gameMulti pos1 pos2 = uncurry max (pairSum (unfoldr stepMulti (p1, p2)))
  where p1 = IntMap.singleton (pos1 - 1) 1
        p2 = IntMap.singleton (pos2 - 1) 1
        pairSum = foldl' (\(a, b) (c, d) -> (a + c, b + d)) (0, 0)

stepMulti :: (IntMap Int, IntMap Int) -> Maybe ((Int, Int), (IntMap Int, IntMap Int))
stepMulti (p1s, p2s) | IntMap.null p1s || IntMap.null p2s = Nothing
stepMulti (p1s, p2s) = Just ((win1, win2), (p1s', p2s'))
  where (win1, p1s') = advanceMulti p1s (sum (IntMap.elems p2s))
        (win2, p2s') = advanceMulti p2s (sum (IntMap.elems p1s'))

advanceMulti :: IntMap Int -> Int -> (Int, IntMap Int)
advanceMulti turnPlayer otherPlayers = (sum wins * otherPlayers, turnPlayer')
  where turnPlayer' = IntMap.fromListWith (+) notWins
        (wins, notWins) = partitionEithers [branchSum (p, roll) | p <- IntMap.assocs turnPlayer, roll <- d33]
        branchSum ((p, freq), (roll, rollfreq)) = let (score', pos') = advance roll (p `quotRem` 10) in
          if score' >= 21 then Left (freq * rollfreq) else Right (score' * 10 + pos', freq * rollfreq)

d33 :: [(Int, Int)]
d33 = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

main :: IO ()
main = do
  s <- readInputFile
  let both f (x, y) = (f x, f y)
      (pos1, pos2) = case lines s of
        [x, y] -> both (read . last . words) (x, y)
        _ -> error "bad players"
  print (gameDet pos1 pos2)
  print (gameMulti pos1 pos2)
