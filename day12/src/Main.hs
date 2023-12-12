module Main where

import Control.Arrow ((&&&))
import Data.List (intercalate)
import Data.Array.IArray as A

type Input = [(String, [Int])]

possibilities :: (String, [Int]) -> Int
possibilities (s, lab) = subproblems A.! (0, 0)
  where s' = s <> "."
        subproblems :: A.Array (Int, Int) Int
        subproblems = A.array ((0, 0), (h, w)) $ do
          y <- [0..h]
          x <- [0..w]
          pure ((y, x), subproblem y x)
          where h = length s'
                w = length lab
        subproblem a b = case (drop a s', drop b lab) of
          ([], []) -> 1
          ([], (_:_)) -> 0
          ((x:xs), ys) -> case x of
            '.' -> skip
            '#' -> use
            '?' -> skip + use
            c -> error $ "Unexpected " <> show c
            where skip = subproblems A.! (a + 1, b)
                  use = case ys of
                    [] -> 0
                    (y:_) -> let expected = replicate (y - 1) '#' <> "."
                                 prefix = take y xs
                                 match p '?' = p
                                 match _ q = q
                                 actual = zipWith match expected prefix
                             in if actual == expected
                                then subproblems A.! (a + y + 1, b + 1)
                                else 0

part1 :: Input -> Int
part1 = sum . map possibilities

part2 :: Input -> Int
part2 = sum . map (possibilities . unfold)
  where unfold (s, lab) = (intercalate "?" $ replicate 5 s, concat $ replicate 5 lab)

prepare :: String -> Input
prepare = map parse . lines
  where parse s = case break (== ' ') s of
          (springs, (' ':label)) -> (springs, read $ ("[" <> label <> "]"))
          _ -> error "no parse"

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
