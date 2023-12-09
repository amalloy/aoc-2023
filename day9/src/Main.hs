{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Maybe (mapMaybe)

import Text.Regex.Applicative (sym, (=~), some)
import Text.Regex.Applicative.Common (signed, decimal)

distill :: [Int] -> [Int]
distill = zipWith subtract <*> tail

solve :: [Int] -> [[Int]]
solve = takeWhile (any (/= 0)) . iterate distill

extrapolate :: [[Int]] -> Int
extrapolate = last . foldr combine (repeat 0)
  where combine [] _ = error "Math is broken"
        combine xs@(x:_) ys = x : zipWith (+) xs ys

type Input = [[Int]]

part1 :: Input -> Int
part1 = sum . map (extrapolate . solve)

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = mapMaybe (=~ input) . lines
  where input = signed decimal `sepBy` sym ' '
        a `sepBy` b = (:) <$> a <*> some (b *> a)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
