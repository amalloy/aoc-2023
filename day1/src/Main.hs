{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (asum)
import Control.Arrow ((&&&))

import Data.Char (isDigit, digitToInt, intToDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)

type Input = [String]

solve :: (b -> Maybe Int) -> (a -> [b]) -> [a] -> Int
solve f g = sum . map (calibrationValue . mapMaybe f . g)
  where calibrationValue = ((+) <$> ((* 10) . head) <*> last)

part1 :: Input -> Int
part1 = solve (digitToInt `guardedBy` isDigit) id
  where guardedBy :: (a -> b) -> (a -> Bool) -> a -> Maybe b
        guardedBy f p x | p x = Just $ f x
                        | otherwise = Nothing

part2 :: Input -> Int
part2 = solve decodeDigit tails
  where decodeDigit :: String -> Maybe Int
        decodeDigit s = asum  . map find $ digits
          where find (k, v) | k `isPrefixOf` s = Just v
                            | otherwise = Nothing
        digits = [ ("one", 1), ("two", 2), ("three", 3), ("four", 4)
                 , ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)
                 ]
                 <>
                 [(pure (intToDigit x), x) | x <- [1..9]]

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
