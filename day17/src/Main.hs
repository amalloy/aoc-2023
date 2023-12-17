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

neighbors :: Node -> [Node]
neighbors (Node p d m) = do
  d' <- [unit _x, unit _y, negated $ unit _x, negated $ unit _y]
  guard $ d' /= negated d
  pure $ Node (p + d') d' (if d == d' then (m + 1) else 1)

cost :: Grid -> Node -> Node -> Cost
cost g _ dest = g A.! _pos dest

aStarHeuristic :: Grid -> Node -> Cost
aStarHeuristic g = go
  where go n = sum $ goal - _pos n
        (_, goal) = A.bounds g

illegal :: Grid -> Node -> Bool
illegal g = go
  where b = A.bounds g
        go n = (not $ A.inRange b (_pos n)) || _momentum n > 3

solved :: Grid -> Node -> Bool
solved g = ((== goal) . _pos)
  where (_, goal) = A.bounds g

type Input = Grid

part1 :: Input -> Maybe Int
part1 g = fmap fst . aStar nexts (cost g) (aStarHeuristic g) (solved g) $ start
  where nexts = neighbors `pruning` (illegal g)
        start = Node (V2 1 1) (V2 0 0) 0

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = mkArray . map (fromMaybe (error "No parse") . (=~ some digit)) . lines
  where mkArray rows = A.listArray (V2 1 1, V2 (length rows) (length $ head rows)) . concat $ rows

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
