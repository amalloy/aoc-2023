{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.List (transpose)
import Data.Maybe (fromMaybe, mapMaybe)

import Text.Regex.Applicative (RE, (=~), many, some, (<|>))

type Parser a = RE Char a

data Feature = Ash | Rocks deriving (Show, Eq, Ord, Enum)

axisOfSymmetry :: Eq a => Int -> [[a]] -> Maybe Int
axisOfSymmetry wantedErrors xss = head $ do
  i <- [1..]
  case splitAt i xss of
    (xs@(_:_), ys@(_:_)) -> do let counterparts = zip (reverse xs) ys
                                   above = map fst counterparts
                                   below = map snd counterparts
                               guard . (== wantedErrors) $ errors above below
                               [Just i]
                                 where errors = (sum .) . zipWith rowErrors
                                       rowErrors = ((length . filter not) .) . zipWith (==)
    _ -> [Nothing]


type Pattern = [[Feature]]
type Input = [Pattern]

solve :: Int -> Input -> Int
solve smudges pats = let results = map go pats
                         failures = filter ((== Nothing) . snd) . zip [0..] $ results
                     in case failures of
                          [] -> sum . mapMaybe id $ results
                          ((i, _):_) -> error $ "Couldn't solve " <> show (i :: Int)
  where go pat = fmap (100 *) (match pat) <|> match (transpose pat)
        match = axisOfSymmetry smudges

part1 :: Input -> Int
part1 = solve 0

part2 :: Input -> Int
part2 = solve 1

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = pattern `sepBy` "\n"
        feature = (Ash <$ ".") <|> (Rocks <$ "#")
        pattern = some line
        line = some feature <* "\n"
        p `sepBy` sep = (:) <$> p <*> many (sep *> p)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
