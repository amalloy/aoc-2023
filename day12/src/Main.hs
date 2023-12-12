{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.List (group)

type Input = [(String, [Int])]

part1 :: Input -> Int
part1 = sum . map possibilities
  where possibilities (s, lab) = length . filter (== lab) . map groups . traverse options $ s
        options '?' = ".#"
        options x = pure x
        groups = map length . filter ((== '#') . head) . group

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = map parse . lines
  where parse s = case break (== ' ') s of
          (springs, (' ':label)) -> (springs, read $ ("[" <> label <> "]"))
          _ -> error "no parse"

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
