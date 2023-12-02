{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

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

range :: (Enum a, Bounded a) => [a]
range = [minBound..maxBound]

type Parser a = RE Char a

space :: Parser ()
space = () <$ sym ' '

sepBy :: Alternative f => f a -> f b -> f [a]
p `sepBy` sep = (:) <$> p <*> many (sep *> p)

color :: Parser Color
color = asum [c <$ string (map toLower (show c)) | c <- range]

oneColorPull :: Parser Pull
oneColorPull = go <$> decimal <* space <*> color
  where go num c = M.singleton c num

pull :: Parser Pull
pull = M.unionsWith (+) <$> oneColorPull `sepBy` string ", "

game :: Parser (Game [Pull])
game = Game <$> (string "Game " *> decimal) <* string ": " <*> pull `sepBy` string "; "

type Input = [Game [Pull]]

gid :: Game a -> Int
gid (Game g _) = g

part1 :: Input -> Int
part1 = sum . map gid . filter possible . map combine
  where possible (Game _ p) = M.unionWith max p limit == limit
        limit = M.fromList [(Red, 12), (Green, 13), (Blue, 14)]
        combine = fmap (M.unionsWith max)

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = mapMaybe (=~ game) . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
