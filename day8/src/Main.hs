{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
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

part1 :: Input -> Int
part1 (Input dirs m) = length . takeWhile (/= Node "ZZZ") . scanl go (Node "AAA") $ cycle dirs
  where go loc dir = let Choice l r = m M.! loc
                     in case dir of
                          L -> l
                          R -> r

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
