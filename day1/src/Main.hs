{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))

import Data.Char (isDigit, digitToInt)

type Input = [String]

part1 :: Input -> Int
part1 = sum . map calibrationValue
  where calibrationValue = ((+) <$> ((* 10) . head) <*> last)
          . map digitToInt . filter isDigit

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
