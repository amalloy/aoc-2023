{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))

import Data.Monoid (Sum(..))
import Data.Set qualified as S

data Coord = Coord {_y, _x :: Int} deriving (Show, Eq, Ord)
type Census = (S.Set Int, S.Set Int)

type Input = S.Set Coord

census :: S.Set Coord -> Census
census = foldMap go
  where go (Coord y x) = (S.singleton y, S.singleton x)

solve :: Int -> Input -> Int
solve factor u = (`div` 2) . getSum . foldMap (uncurry distance) $ S.cartesianProduct u u
  where distance (Coord y1 x1) (Coord y2 x2) =
          foldMap Sum [ abs (y1 - y2)
                      , abs (x1 - x2)
                      , between (fst c) (max y1 y2) (min y1 y2)
                      , between (snd c) (max x1 x2) (min x1 x2)
                      ]
          where c = census u
                between s hi lo = (factor - 1) * (unpopBelow hi - unpopBelow lo)
                  where unpopBelow n = let (below, _) = S.split n s
                                       in n - S.size below

part1 :: Input -> Int
part1 = solve 2

part2 :: Input -> Int
part2 = solve 1000000

prepare :: String -> Input
prepare text = S.fromList $ do
  (y, row) <- zip [0..] (lines text)
  (x, c) <- zip [0..] row
  case c of
    '#' -> pure $ Coord y x
    '.' -> mempty
    _ -> error $ "Unexpected " <> pure c

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
