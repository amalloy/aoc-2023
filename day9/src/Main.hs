{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)

import Text.Regex.Applicative (sym, (=~), some)
import Text.Regex.Applicative.Common (signed, decimal)

distill :: [Int] -> [Int]
distill = zipWith subtract <*> tail

solve :: [Int] -> [[Int]]
solve = takeWhile (any (/= 0)) . iterate distill

extrapolate :: (NonEmpty Int -> NonEmpty Int -> NonEmpty Int) -> [[Int]] -> NonEmpty Int
extrapolate f xs = foldr f zeroes lists
  where zeroes = NE.zipWith const (NE.repeat 0) (last lists)
        lists = map NE.fromList xs

type Input = [[Int]]

part1 :: Input -> Int
part1 = sum . map (NE.last . extrapolate combine . solve)
  where combine xs ys = NE.scanl (+) (NE.head xs) ys

part2 :: Input -> Int
part2 = sum . map (NE.head . extrapolate combine . solve)
  where combine xs ys = NE.scanr subtract (NE.last xs) ys

prepare :: String -> Input
prepare = mapMaybe (=~ input) . lines
  where input = signed decimal `sepBy` sym ' '
        a `sepBy` b = (:) <$> a <*> some (b *> a)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
