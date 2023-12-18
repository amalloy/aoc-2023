{-# LANGUAGE DerivingVia #-}

module Main where

import GHC.Generics (Generic, Generically(..))

import Control.Arrow ((&&&))
import Control.Monad (replicateM, foldM_)
import Control.Monad.Trans.Writer (execWriter, tell)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Last(..), Min(..), Max(..))
import Linear.Vector (unit, basis)
import Linear.V2 (V2(..), _x, _y)

import Text.Regex.Applicative ((=~), some, anySym, sym, string, asum)
import Text.Regex.Applicative.Common (decimal)

import Data.Map.Strict qualified as M
import Data.Set qualified as S

type Direction = V2 Int
type Color = String
data Order = Order Direction Int Color deriving Show
data Bounds = Bounds (V2 (Min Int)) (V2 (Max Int))
  deriving (Show, Generic)
  deriving Semigroup via Generically Bounds

type Trench f = M.Map (V2 Int) (f Color)

digTrench :: (Applicative f, Semigroup (f Color)) => [Order] -> Trench f
digTrench = M.fromListWith (<>) . execWriter . foldM_ go (V2 0 0)
  where go pos (Order dir len color) = do
          tell [ (pos + dir * pure n, pure color) | n <- [0..len]]
          pure $ pos + dir * pure len

bounds :: Trench f -> Bounds
bounds = fromMaybe (Bounds 0 0) . foldMap go . M.keys
  where go c = Just $ Bounds (coerce c) (coerce c)

type Input = [Order]

part1 :: Input -> Int
part1 = lavaArea . digTrench @Last
  where lavaArea t = landArea - S.size nonLavaArea
          where (Bounds (V2 (Min minY) (Min minX)) (V2 (Max maxY) (Max maxX))) = bounds t
                landArea = (maxY - minY + 1) * (maxX - minX + 1)
                nonLavaArea = go S.empty . concat $ [west, north, east, south]
                  where xs = [minX..maxX]
                        ys = [minY..maxY]
                        west = flip V2 minX <$> ys
                        east = flip V2 maxX <$> ys
                        north = V2 minY <$> xs
                        south = V2 maxY <$> xs
                go seen [] = seen
                go seen (c:cs) | S.member c seen || M.member c t = prune
                               | otherwise = case c of
                                   V2 y x | y < minY || y > maxY || x < minX || x > maxX -> prune
                                          | otherwise -> go (S.insert c seen) ((fmap (c +) $ [negate, id] <*> basis) <> cs)
                  where prune = go seen cs

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = some (order <* sym '\n')
        order = Order <$> direction <* sym ' ' <*> decimal <* sym ' ' <*> color
        color = string "(#" *> replicateM 6 anySym <* sym ')'
        direction = asum [ (- unit _y) <$ sym 'U'
                         , (- unit _x) <$ sym 'L'
                         , unit _y <$ sym 'D'
                         , unit _x <$ sym 'R'
                         ]

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
