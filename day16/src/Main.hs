{-# LANGUAGE DerivingVia #-}

module Main where

import GHC.Generics (Generic1, Generically1(..))

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))

import Data.Bits (xor)
import Data.Foldable (toList)
import Data.Ix (Ix, inRange)
import Data.Maybe (fromMaybe)

import Data.Array.IArray qualified as A
import Data.Set qualified as S

import Text.Regex.Applicative ((=~), (<|>), some, sym)

data Coord a = Coord {_y, _x :: a} deriving (Show, Eq, Ord, Ix, Generic1, Functor)
  deriving Applicative via Generically1 Coord
data Direction = North | East | South | West deriving (Show, Eq, Ord, Enum)
data Trace = Trace {_coord :: Coord Int, _dir :: Direction} deriving (Show, Eq, Ord)

data Some a = One a | Two a a deriving (Show, Functor, Foldable, Traversable)
data Feature = Empty | Mirror (Direction -> Direction) | Splitter (Direction -> Some Direction)

unit :: Direction -> Coord Int
unit North = Coord (-1) 0
unit South = Coord 1 0
unit East = Coord 0 1
unit West = Coord 0 (-1)

nexts :: Feature -> Direction -> Some Direction
nexts Empty = One
nexts (Mirror m) = One . m
nexts (Splitter s) = s

type Input = A.Array (Coord Int) Feature

step :: Input -> Trace -> Some Trace
step g (Trace c d) = fmap go $ nexts (g A.! c) d
  where go d' = Trace (liftA2 (+) c (unit d')) d'

stepAll :: Input -> [Trace] -> [Trace]
stepAll g = filter inBounds . (>>= (toList . step g))
  where inBounds (Trace c _) = inRange r c
        r = A.bounds g

energizedBy :: Input -> Trace -> Int
energizedBy g = S.size . S.map _coord . go S.empty . pure
  where go seen ts = case filter (not . flip S.member seen) ts of
          [] -> seen
          xs -> go (S.union seen (S.fromList xs)) (stepAll g xs)

part1 :: Input -> Int
part1 g = energizedBy g (Trace (Coord 0 0) East)

part2 :: Input -> Int
part2 g = maximum . map (energizedBy g) $ starts
  where starts = concat [ [ Trace (Coord y 0) East | y <- [0..maxY]]
                        , [ Trace (Coord y maxX) West | y <- [0..maxY]]
                        , [ Trace (Coord 0 x) South | x <- [0..maxX]]
                        , [ Trace (Coord maxY x) North | x <- [0..maxX]]
                        ]
        (_, Coord maxY maxX) = A.bounds g

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
