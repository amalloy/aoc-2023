{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (Alternative, asum, many)
import Control.Arrow ((&&&))
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Text.Regex.Applicative (RE, sym, string, (=~))
import Text.Regex.Applicative.Common (decimal)

data Color = Red | Green | Blue deriving (Show, Eq, Ord, Enum, Bounded)
type Pull = M.Map Color Int
data Game a = Game Int a deriving (Show, Functor)
type Input = [Game [Pull]]

type Parser a = RE Char a

sepBy :: Alternative f => f a -> f b -> f [a]
p `sepBy` sep = (:) <$> p <*> many (sep *> p)

color :: Parser Color
color = asum [c <$ string (map toLower (show c)) | c <- [minBound..maxBound]]

oneColorPull :: Parser Pull
oneColorPull = flip M.singleton <$> decimal <* sym ' ' <*> color

pull :: Parser Pull
pull = M.unionsWith (+) <$> oneColorPull `sepBy` string ", "

game :: Parser (Game [Pull])
game = Game <$> (string "Game " *> decimal) <* string ": " <*> pull `sepBy` string "; "

combine :: Game [Pull] -> Game Pull
combine = fmap (M.unionsWith max)

part1 :: Input -> Int
part1 = sum . mapMaybe (score . combine)
  where score (Game gid p) | M.unionWith max p limit == limit = Just gid
                           | otherwise = Nothing
        limit = M.fromList [(Red, 12), (Green, 13), (Blue, 14)]

part2 :: Input -> Int
part2 = sum . map (power . combine)
  where power (Game _ p) = product (M.elems p)

prepare :: String -> Input
prepare = mapMaybe (=~ game) . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
