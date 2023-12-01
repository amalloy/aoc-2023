{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))

import Data.Char (isDigit, digitToInt, intToDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)
import Data.Monoid (First(..))

type Input = [String]

calibrationValue :: [Int] -> Int
calibrationValue = ((+) <$> ((* 10) . head) <*> last)

part1 :: Input -> Int
part1 = sum . map (calibrationValue . map digitToInt . filter isDigit)

digits :: [(String, Int)]
digits = [ ("one", 1), ("two", 2), ("three", 3), ("four", 4)
         , ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)
         ] <>
         [(pure (intToDigit x), x) | x <- [1..9]]

part2 :: Input -> Int
part2 = sum . map (calibrationValue . (mapMaybe decodeDigit . tails))
  where decodeDigit :: String -> Maybe Int
        decodeDigit s = getFirst . mconcat . map (First . find) $ digits
          where find (k, v) | k `isPrefixOf` s = Just v
                            | otherwise = Nothing

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
