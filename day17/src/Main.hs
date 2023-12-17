module Main where

import Algorithm.Search (aStar, pruning)

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Linear.Vector (unit, negated)
import Linear.V2 (V2(..), _x, _y)

import Text.Regex.Applicative ((=~), some)
import Text.Regex.Applicative.Common (digit)

import Data.Array.IArray qualified as A

type Cost = Int
type Grid = A.Array (V2 Int) Cost

data Node = Node {_pos, _dir :: V2 Int, _momentum :: Int} deriving (Show, Eq, Ord)
data Wobbliness = Wobbliness {_minMomentum, _maxMomentum :: Int}

neighbors :: Wobbliness -> Node -> [Node]
neighbors (Wobbliness lo _) (Node p d m) | m < lo && d /= 0 = go d
                                         | otherwise = do
  d' <- [unit _x, unit _y, negated $ unit _x, negated $ unit _y]
  guard $ d' /= negated d
  go d'
  where go d' = pure $ Node (p + d') d' (if d == d' then (m + 1) else 1)

cost :: Grid -> Node -> Node -> Cost
cost g _ dest = g A.! _pos dest

aStarHeuristic :: Grid -> Node -> Cost
aStarHeuristic g = go
  where go n = sum $ goal - _pos n
        (_, goal) = A.bounds g

illegal :: Wobbliness -> Grid -> Node -> Bool
illegal (Wobbliness _ hi) g = go
  where b = A.bounds g
        go (Node pos _ m) = (not $ A.inRange b pos) || m > hi

solved :: Wobbliness -> Grid -> Node -> Bool
solved (Wobbliness lo _) g = go
  where (_, goal) = A.bounds g
        go (Node p _ m) = m >= lo && p == goal

type Input = Grid

solve :: Wobbliness -> Input -> Maybe Int
solve w g = fmap fst . aStar nexts (cost g) (aStarHeuristic g) (solved w g) $ start
  where nexts = (neighbors w) `pruning` (illegal w g)
        start = Node (V2 1 1) (V2 0 0) 0

part1 :: Input -> Maybe Int
part1 = solve (Wobbliness 0 3)

part2 :: Input -> Maybe Int
part2 = solve (Wobbliness 4 10)

prepare :: String -> Input
prepare = mkArray . map (fromMaybe (error "No parse") . (=~ some digit)) . lines
  where mkArray rows = A.listArray (V2 1 1, V2 (length rows) (length $ head rows)) . concat $ rows

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
