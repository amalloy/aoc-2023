{-# LANGUAGE DerivingVia, LambdaCase #-}

module Main where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Monad.ST (ST, runST)
import Control.Monad (replicateM_)

import Data.Array.ST (STArray)
import Data.Array.MArray (newListArray, readArray, writeArray, getBounds, getAssocs, Ix(..))
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)

import Data.Map.Strict qualified as M

import Text.Regex.Applicative ((=~), some, asum, sym)

import GHC.Generics (Generic1, Generically1(..))

data Coord a = Coord {_y, _x :: a}
  deriving (Show, Eq, Ord, Functor, Generic1, Ix)
  deriving Applicative via Generically1 Coord

data Direction = North | South | East | West deriving (Show, Eq, Ord, Enum)
data Scan = Scan {unit, startPos, normal :: Coord Int -> Coord Int}

scan :: Direction -> Scan
scan d = case d of
  North -> Scan (by (Coord (-1) 0)) origin east
  South -> Scan (by (Coord 1 0)) (liftA2 (*) (Coord 1 0)) east
  East -> Scan (by (Coord 0 1)) (liftA2 (*) (Coord 0 1)) south
  West -> Scan (by (Coord 0 (-1))) origin south
  where by o = liftA2 (+) (fmap negate o)
        origin _ = Coord 0 0
        east = liftA2 (+) $ Coord 0 1
        south = liftA2 (+) $ Coord 1 0

data Feature = Empty | Wall | Boulder deriving (Show, Eq, Ord, Enum)

type Grid s = STArray s (Coord Int) Feature

roll :: Direction -> Grid s -> ST s ()
roll d g = do
  let s = scan d
  bounds@(_, hi) <- getBounds g
  let starts = takeWhile (inRange bounds) $ iterate (normal s) (startPos s hi)
  for_ starts $ \start ->
    let loop curr vacant | not (inRange bounds curr) = pure ()
                         | otherwise = readArray g curr >>= \case
                             Empty -> loop next vacant
                             Wall -> loop next next
                             Boulder -> do
                               writeArray g curr Empty
                               writeArray g vacant Boulder
                               loop next (unit s vacant)

          where next = unit s curr
    in loop start start

showGrid :: Grid s -> ST s [String]
showGrid g = do
  (_, Coord maxY maxX) <- getBounds g
  for [0..maxY] $ \y ->
    for [0..maxX] $ \x ->
      showFeature <$> readArray g (Coord y x)
  where showFeature f = case f of
          Empty -> '.'
          Boulder -> 'O'
          Wall -> '#'

type Input = [[Feature]]

toArray :: Input -> ST s (Grid s)
toArray fs =
  let h = length fs
      w = length (head fs)
  in newListArray (Coord 0 0, Coord (h - 1) (w - 1)) (concat fs)

part1 :: Input -> Int
part1 fs = runST $ do
  g <- toArray fs
  (_, Coord maxY _) <- getBounds g
  let load ((Coord y _), Boulder) = maxY - y + 1
      load _ = 0
  roll North g
  sum . map load <$> getAssocs g

spinCycle :: Int -> Grid s -> ST s ()
spinCycle times g = loop 0 M.empty
  where loop cycleNum prevs | cycleNum == times = pure ()
                            | otherwise = do
                                state <- getAssocs g
                                case M.alterF (insert cycleNum) state prevs of
                                  Left lastSeen -> do
                                    let cycleLength = cycleNum - lastSeen
                                        leftover = (times - cycleNum) `mod` cycleLength
                                    replicateM_ leftover spin
                                  Right prevs' -> do
                                    spin
                                    loop (cycleNum + 1) prevs'
        insert n Nothing = Right (Just n)
        insert _n (Just p) = Left p
        spin = roll North g *> roll West g *> roll South g *> roll East g

part2 :: Input -> Int
part2 fs = runST $ do
  g <- toArray fs
  (_, Coord maxY _) <- getBounds g
  let load ((Coord y _), Boulder) = maxY - y + 1
      load _ = 0
  spinCycle 1000000000 g
  sum . map load <$> getAssocs g

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = some (row <* sym '\n')
        row = some feature
        feature = asum [f <$ sym s | (f, s) <- zip [Empty, Wall, Boulder] ".#O"]

debug :: Input -> String
debug fs = runST $ do
  g <- toArray fs
  spinCycle 2 g
  unlines <$> showGrid g

main :: IO ()
main = do
  i <- prepare <$> readFile "input.txt"
  print . (part1 &&& part2) $ i
  putStr =<< debug . prepare <$> readFile "input.test"
