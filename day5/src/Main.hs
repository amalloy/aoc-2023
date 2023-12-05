{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (some)
import Control.Arrow ((&&&))

import Data.Map.Strict qualified as M
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative (RE, sym, string, psym, (=~))
import Text.Regex.Applicative.Common (decimal)

data Range = Range {_source, _dest, _size :: Int} deriving Show
type Mapping = IM.IntMap Range
type Ingredient = String
data Page = Page {_from, _to :: Ingredient, _ranges :: Mapping} deriving Show
type Index = M.Map Ingredient Page
data Almanac = Almanac [Int] Index deriving Show

translate :: Mapping -> Int -> Int
translate m q = maybe q go . IM.lookupLE q $ m
  where go (_, Range source dest size) | source + size > q = dest + (q - source)
                                       | otherwise = q

type Parser a = RE Char a

range :: Parser Range
range = flip Range <$> decimal <* sym ' ' <*> decimal <* sym ' ' <*> decimal

mapping :: Parser Mapping
mapping = IM.fromList . map fromRange <$> some (range <* sym '\n')
  where fromRange r@(Range src _ _) = (src, r)

page :: Parser Page
page = do
  sym '\n'
  src <- ingredient
  string "-to-"
  dst <- ingredient
  string " map:\n"
  m <- mapping
  pure (Page src dst m)
  where ingredient = some (psym (/= '-'))

almanac :: Parser Almanac
almanac = Almanac <$> seeds <*> (collate <$> some page)
  where seeds = string "seeds:" *> some (sym ' ' *> decimal) <* string "\n"
        collate pages = M.fromList [(src, entry) | entry@(Page src _ _) <- pages]

location :: Index -> Int -> Int
location idx = go "seed"
  where go ingredient n = maybe n handle (M.lookup ingredient idx)
          where handle (Page _ to next) = go to (translate next n)

type Input = Almanac

part1 :: Input -> Int
part1 (Almanac seeds idx) = minimum . map (location idx) $ seeds

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ almanac)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
