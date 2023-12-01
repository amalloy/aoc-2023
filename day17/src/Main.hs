{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))

type Input = [String]

part1 :: Input -> ()
part1 = const ()

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
