{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}

module Main where

import GHC.Generics (Generic1, Generically1(..))

import Control.Arrow ((&&&))
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)

import Data.Map.Strict qualified as M

import Text.Regex.Applicative (RE, sym, (=~), asum)
import Text.Regex.Applicative.Common (decimal)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord, Enum)

newtype JokerJack = JokerJack Value deriving (Show, Eq)
instance Ord JokerJack where
  JokerJack Jack <= JokerJack _ = True
  JokerJack _ <= JokerJack Jack = False
  JokerJack a <= JokerJack b = a <= b

data Hand a = Hand a a a a a
  deriving (Functor, Foldable, Traversable, Show, Eq, Ord, Generic1)
  deriving Applicative via Generically1 Hand

data Kind = HighCard
          | OnePair
          | TwoPair
          | ThreeOfAKind
          | FulLHouse
          | FourOfAKind
          | FiveOfAKind
          deriving (Show, Eq, Ord)
data Game a = Game (Hand a) Int deriving (Show, Functor)

type Parser a = RE Char a

value :: Parser Value
value = asum . zipWith (\val label -> val <$ sym label) [Two ..] $ "23456789TJQKA"

hand :: Parser (Hand Value)
hand = sequenceA $ pure value

game :: Parser (Game Value)
game = Game <$> (hand <* sym ' ') <*> decimal

frequencies :: Ord a => [a] -> M.Map a Int
frequencies = M.fromListWith (+) . flip zip (repeat (1 :: Int))

templates :: M.Map [Int] Kind
templates = M.fromList [ ([5], FiveOfAKind)
                       , ([4, 1], FourOfAKind)
                       , ([3, 2], FulLHouse)
                       , ([3, 1, 1], ThreeOfAKind)
                       , ([2, 2, 1], TwoPair)
                       , ([2, 1, 1, 1], OnePair)
                       , ([1, 1, 1, 1, 1], HighCard)
                       ]
kind :: Hand Value -> Kind
kind = (templates M.!) . sortOn negate . M.elems . frequencies . toList

jokerKind :: Hand JokerJack -> Kind
jokerKind h = let freqs = frequencies . toList $ h
                  jokers = M.findWithDefault 0 (JokerJack Jack) freqs
                  others = M.elems (M.delete (JokerJack Jack) freqs)
                  (best:rest) = case sortOn negate others of
                                  [] -> [0]
                                  xs -> xs
              in templates M.! ((best + jokers) : rest)

type Input = [Game Value]

part1 :: Input -> Int
part1 = sum . zipWith (*) [1..] . M.elems . M.fromList . map entry
  where entry (Game h bid) = ((kind h, h), bid)

part2 :: Input -> Int
part2 = sum . zipWith (*) [1..] . M.elems . M.fromList . map entry
  where entry (Game h bid) = ((jokerKind h', h'), bid)
          where h' = fmap JokerJack h

prepare :: String -> Input
prepare = mapMaybe (=~ game) . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
