module Main where

import Algorithm.Search (aStar, pruning)

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Linear.Vector (basis)
import Linear.V2 (V2(..))

import Text.Regex.Applicative ((=~), some)
import Text.Regex.Applicative.Common (digit)

import Data.Array.IArray qualified as A

type Cost = Int
type Grid = A.Array (V2 Int) Cost

data Node = Node {_pos, _dir :: V2 Int} deriving (Show, Eq, Ord)
data Wobbliness = Wobbliness {_minMomentum, _maxMomentum :: Int}

neighbors :: Wobbliness -> Node -> [Node]
neighbors (Wobbliness lo hi) (Node p d) = do
  d' <- [negate, id] <*> basis
  guard $ d' `notElem` [d, negate d]
  len <- [lo..hi]
  pure $ Node (p + (pure len * d')) d'

cost :: Grid -> Node -> Node -> Cost
cost g (Node from _) (Node to _) = sum . map (g A.!) $ enteredCells
  where enteredCells = takeWhile (/= from) . iterate (subtract delta) $ to
        delta = signum (to - from)

aStarHeuristic :: Grid -> Node -> Cost
aStarHeuristic g = go
  where go n = sum $ goal - _pos n
        (_, goal) = A.bounds g

illegal :: Grid -> Node -> Bool
illegal g = outOfBounds
  where outOfBounds = not . A.inRange (A.bounds g) . _pos

solved ::  Grid -> Node -> Bool
solved g = (== snd (A.bounds g)) . _pos

type Input = Grid

solve :: Wobbliness -> Input -> Maybe Int
solve w g = fmap fst . aStar nexts (cost g) (aStarHeuristic g) (solved g) $ start
  where nexts = (neighbors w) `pruning` (illegal g)
        start = Node (V2 1 1) (V2 0 0)

part1 :: Input -> Maybe Int
part1 = solve (Wobbliness 1 3)

part2 :: Input -> Maybe Int
part2 = solve (Wobbliness 4 10)

prepare :: String -> Input
prepare = mkArray . map (fromMaybe (error "No parse") . (=~ some digit)) . lines
  where mkArray rows = A.listArray (V2 1 1, V2 (length rows) (length $ head rows)) . concat $ rows

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
