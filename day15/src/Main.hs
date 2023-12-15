module Main where

import Control.Arrow ((&&&))

import Data.Char (ord)
import Data.List (foldl')
import Data.List.Split (splitOn)

type Input = [String]

hashString :: String -> Int
hashString = foldl' hash 0
  where hash current c = ((current + ord c) * 17) `mod` 256

part1 :: Input -> Int
part1 = sum . map hashString

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = splitOn "," . filter (/= '\n')

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
