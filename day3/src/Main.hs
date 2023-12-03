{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (many, some, asum)
import Control.Arrow ((&&&))
import Control.Monad (foldM_)
import Control.Monad.Trans.Writer (execWriter, tell)

import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S

import Text.Regex.Applicative (RE, psym, (=~))

data Coord = Coord {_x, _y :: Int} deriving (Show, Eq, Ord)
newtype Symbol = Symbol Char deriving (Show, Eq, Ord)
data Measured a = Measured Int a deriving (Show, Functor)
data Located a = Located (S.Set Coord) a deriving (Show, Eq, Ord, Functor)

type Input = (S.Set (Located Int), M.Map Coord Symbol)

neighbors :: Coord -> S.Set Coord
neighbors (Coord x y) = S.fromList $ do
  dx <- [-1..1]
  dy <- [-1..1]
  pure $ Coord (x + dx) (y + dy)

type Parser a = RE Char a

measured :: (Char -> Bool) -> (String -> a) -> Parser (Measured a)
measured p f = do
  region <- some (psym p)
  pure (Measured (length region) (f region))

data Token = TokInt Int | TokBlank | TokSym Symbol deriving (Show, Eq, Ord)

number, blank, symbol :: Parser (Measured Token)
number = measured isDigit (TokInt . read)
blank = measured (== '.') (const TokBlank)
symbol = Measured 1 . TokSym . Symbol <$> psym (not . ((||) <$> (== '.') <*> isDigit))

line :: Parser [Measured Token]
line = many (asum [number, blank, symbol])

collate :: [[Measured Token]] -> Input
collate = execWriter . traverse_ row . zip [0..]
  where row (y, r) = foldM_ include 0 r
          where include x (Measured width tok) = do
                  case tok of
                    TokBlank -> pure ()
                    TokSym s -> tell (mempty, M.singleton (Coord x y) s)
                    TokInt i -> tell (S.singleton deflated, mempty)
                      where deflated = Located coords i
                            coords = S.fromList $ do
                              dx <- [0..width-1]
                              pure $ Coord (x + dx) y
                  pure $ x + width

part1 :: Input -> Int
part1 (nums, syms) = sum . mapMaybe part . S.toList $ nums
  where symLocs = M.keysSet syms
        part (Located coords n) | S.null $ S.intersection symLocs (foldMap neighbors coords) = Nothing
                                | otherwise = Just n

part2 :: Input -> Int
part2 (nums, syms) = sum . mapMaybe gearRatio . M.assocs $ syms
  where gearRatio (loc, Symbol '*') = case filter adjacent nums' of
          [Located _ x, Located _ y] -> Just $ x * y
          _ -> Nothing
          where adjacent (Located coords _) = S.member loc (foldMap neighbors coords)
        gearRatio _ = Nothing
        nums' = S.toList nums

prepare :: String -> Input
prepare = collate . mapMaybe (=~ line) . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
