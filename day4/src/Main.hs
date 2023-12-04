{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (some)
import Control.Arrow ((&&&))

import Data.IntMap.Lazy qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S

import Text.Regex.Applicative (RE, sym, string, (=~))
import Text.Regex.Applicative.Common (decimal)

data Numbered a = Numbered Int a deriving (Functor, Foldable, Show, Eq, Ord)
data Card = Card {_winning, _ours :: [Int]} deriving (Show)

type Input = [Numbered Card]

type Parser a = RE Char a

spaces :: Parser ()
spaces = () <$ some (sym ' ')

card :: Parser Card
card = Card <$> some number <* string " |" <*> some number
  where number = spaces *> decimal

line :: Parser (Numbered Card)
line = Numbered <$> (string "Card" *> spaces *> decimal <* sym ':') <*> card

part1 :: Input -> Int
part1 = sum . map value
  where value (Numbered _ (Card winning got)) | S.null winners = 0
                                              | otherwise = 2 ^ (S.size winners - 1)
          where winners = S.intersection (S.fromList winning) (S.fromList got)

part2 :: Input -> Int
part2 cards = M.size netCards + (sum (M.elems netCards))
  where netCards = M.fromList entries
        entries = do
          Numbered cardNum (Card winning got) <- cards
          let numWinners = S.size $ S.intersection (S.fromList winning) (S.fromList got)
          pure (cardNum, numWinners + sum [netCards M.! (cardNum + i) | i <- [1..numWinners]])


prepare :: String -> Input
prepare = mapMaybe (=~ line) . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
