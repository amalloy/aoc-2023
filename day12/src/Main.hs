module Main where

import Control.Arrow ((&&&))
import Data.List (group, intercalate)
import Data.Map.Lazy qualified as M

type Input = [(String, [Int])]

part1 :: Input -> Int
part1 = sum . map possibilities
  where possibilities (s, lab) = length . filter (== lab) . map groups . traverse options $ s
        options '?' = ".#"
        options x = pure x
        groups = map length . filter ((== '#') . head) . group

unfold :: (String, [Int]) -> (String, [Int])
unfold (s, lab) = (intercalate "?" $ replicate 5 s, concat $ replicate 5 lab)

part2 :: Input -> Int
part2 = sum . map (possibilities . unfold)
  where possibilities (s, lab) = subproblems M.! (0, 0)
          where s' = s <> "."
                subproblems = M.fromList $ do
                  a <- [0..length s']
                  b <- [0..length lab]
                  pure ((a, b), subproblem a b)
                subproblem a b = case (drop a s', drop b lab) of
                  ([], []) -> 1
                  ([], (_:_)) -> 0
                  ((x:xs), ys) -> case x of
                    '.' -> skip
                    '#' -> use
                    '?' -> skip + use
                    c -> error $ "Unexpected " <> show c
                    where skip = subproblems M.! (a + 1, b)
                          use = case ys of
                            [] -> 0
                            (y:_) -> let expected = replicate (y - 1) '#' <> "."
                                         prefix = take y xs
                                         match p '?' = p
                                         match _ q = q
                                         actual = zipWith match expected prefix
                                     in if actual == expected
                                        then subproblems M.! (a + y + 1, b + 1)
                                        else 0

prepare :: String -> Input
prepare = map parse . lines
  where parse s = case break (== ' ') s of
          (springs, (' ':label)) -> (springs, read $ ("[" <> label <> "]"))
          _ -> error "no parse"

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
