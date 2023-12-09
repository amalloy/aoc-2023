{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (repeat, last, init)

import Control.Arrow ((&&&))
import Data.List.NonEmpty (NonEmpty(..), repeat, toList, last, init, fromList)
import Data.Maybe (mapMaybe)

import Text.Regex.Applicative (sym, (=~), some)
import Text.Regex.Applicative.Common (signed, decimal)

distill :: [Int] -> [Int]
distill = zipWith subtract <*> tail

solve :: [Int] -> [[Int]]
solve = takeWhile (any (/= 0)) . iterate distill

extrapolate :: (NonEmpty Int -> NonEmpty Int -> NonEmpty Int) -> [[Int]] -> Int
extrapolate f = last . foldr f (repeat 0) . map fromList

type Input = [[Int]]

part1 :: Input -> Int
part1 = sum . map (extrapolate combine . solve)
  where combine (x :| xs) ys = x :| zipWith (+) xs (toList ys)

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = mapMaybe (=~ input) . lines
  where input = signed decimal `sepBy` sym ' '
        a `sepBy` b = (:) <$> a <*> some (b *> a)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
