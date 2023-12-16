{-# LANGUAGE DerivingVia #-}
module Main where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))

import Data.Array.IArray qualified as A
import Data.Bits (xor)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.Ix (Ix, inRange)
import Data.Maybe (fromMaybe)

import Data.Set qualified as S

import Text.Regex.Applicative ((=~), (<|>), some, sym)

import GHC.Generics (Generic1, Generically1(..))

data Direction = North | East | South | West deriving (Show, Eq, Ord, Enum)

data Coord a = Coord {_y, _x :: a} deriving (Show, Eq, Ord, Ix, Generic1, Functor)
  deriving Applicative via Generically1 Coord

data Some a = One a | Two a a deriving (Show, Functor, Foldable, Traversable)

data Feature = Empty | Mirror (Direction -> Direction) | Splitter (Direction -> Some Direction)

data Trace = Trace (Coord Int) Direction deriving (Show, Eq, Ord)

unit :: Direction -> Coord Int
unit North = Coord (-1) 0
unit South = Coord 1 0
unit East = Coord 0 1
unit West = Coord 0 (-1)

nexts :: Feature -> Direction -> Some Direction
nexts Empty = One
nexts (Mirror m) = One . m
nexts (Splitter s) = s

step :: Input -> Trace -> Some Trace
step g (Trace c d) = fmap go $ nexts (g A.! c) d
  where go d' = Trace (liftA2 (+) c (unit d')) d'

stepAll :: Input -> [Trace] -> [Trace]
stepAll g = nubOrd . filter inBounds . (>>= (toList . step g))
  where inBounds (Trace c _) = inRange r c
        r = A.bounds g

runTillRepeat :: Ord a => (a -> a) -> a -> [a]
runTillRepeat next = go S.empty
  where go seen x | x `S.member` seen = []
                  | otherwise = x : go (S.insert x seen) (next x)

type Input = A.Array (Coord Int) Feature

part1 :: Input -> Int
part1 g = S.size . foldMap actives . runTillRepeat (stepAll g) $ [Trace (Coord 0 0) East]
  where actives traces = S.fromList . map coord $ traces
          where coord (Trace c _) = c

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare s = mkArray . fromMaybe (error "no parse") . (=~ input) $ s
  where mkArray fss = A.listArray (Coord 0 0, Coord (h - 1) (w - 1)) $ concat fss
          where h = length fss
                w = length (head fss)
        input = some (line <* sym '\n')
        line = some feature
        feature = empty <|> mirror <|> splitter
        empty = Empty <$ sym '.'
        mirror = Mirror . reflect <$> ((0b01 <$ sym '/') <|> (0b11 <$ sym '\\'))
          where reflect mask = toEnum . xor mask . fromEnum
        splitter = Splitter . splitter' <$> (([West, East] <$ sym '-') <|> ([North, South] <$ sym '|'))
          where splitter' dirs d | d `elem` dirs = One d
                                 | otherwise = Two (go (+ 1)) (go (subtract 1))
                  where go f = toEnum $ f (fromEnum d) `mod` 4

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
