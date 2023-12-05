{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (some)
import Control.Arrow ((&&&))

import Data.IntMap.Strict qualified as M

import Text.Regex.Applicative (RE, sym, string, psym, (=~))
import Text.Regex.Applicative.Common (decimal)

type Mapping = M.IntMap Range
data Range = Range {_source, _dest, _size :: Int} deriving Show
data Page = Page {_from, _to :: Ingredient, _ranges :: Mapping} deriving Show

type Ingredient = String

translate :: Mapping -> Int -> Int
translate m q = maybe q go . M.lookupLE q $ m
  where go (_, Range source dest size) | source + size > q = dest + (q - source)
                                       | otherwise = q

type Parser a = RE Char a

range :: Parser Range
range = flip Range <$> decimal <* sym ' ' <*> decimal <* sym ' ' <*> decimal

mapping :: Parser Mapping
mapping = M.fromList . map fromRange <$> some (range <* sym '\n')
  where fromRange r@(Range src _ _) = (src, r)

page :: Parser Page
page = do
  src <- ingredient
  string "-to-"
  dst <- ingredient
  string " map:\n"
  m <- mapping
  pure (Page src dst m)
  where ingredient = some (psym (/= '-'))

almanac :: Parser Almanac
almanac = Almanac <$> seeds <*> some page
  where seeds = string "seeds:" *> some (sym ' ' *> decimal) <* string "\n\n"

data Almanac = Almanac [Int] [Page] deriving Show
type Input = [String]

part1 :: Input -> ()
part1 = const ()

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
