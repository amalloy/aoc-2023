{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative (RE, (=~), string, psym, sym, some, (<|>))

import Data.Map.Strict qualified as M
import Data.Set qualified as S

newtype Node = Node String deriving (Show, Eq, Ord)
data Choice a = Choice a a deriving (Show)
data Direction = L | R deriving (Show)
type Atlas = M.Map Node (Choice Node)

type Parser a = RE Char a

direction :: Parser Direction
direction = (L <$ sym 'L') <|> (R <$ sym 'R')

punctuation :: Char -> Bool
punctuation = flip S.member ps
  where ps = S.fromList " =(),"

node :: Parser Node
node = Node <$> some (psym (not . punctuation))

line :: Parser (Node, Choice Node)
line = (,) <$> node <* string " = (" <*> choice
  where choice = Choice <$> node <* string ", " <*> node <* sym ')'

data Input = Input [Direction] Atlas deriving Show

input :: Parser Input
input = Input <$> some direction <* string "\n\n" <*> (M.fromList <$> some (line <* sym '\n'))

walk :: Node -> Input -> [Node]
walk start (Input dirs m) = scanl go start $ cycle dirs
  where go loc dir = let Choice l r = m M.! loc
                     in case dir of
                          L -> l
                          R -> r

part1 :: Input -> Int
part1 = length . takeWhile (/= Node "ZZZ") . walk (Node "AAA")

part2 :: Input -> Int
part2 i@(Input _ m) = foldr lcm 1 . map loopLength . filter (endsWith "A") . M.keys $ m
  where loopLength = length . takeWhile (not . endsWith "Z") . flip walk i
        endsWith s (Node n) = s `isSuffixOf` n

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
