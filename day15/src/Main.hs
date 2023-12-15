module Main where

import Control.Arrow ((&&&))

import Data.Char (ord, digitToInt)
import Data.Foldable (traverse_, toList)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (Sum(..))
import Data.Traversable (mapAccumL)
import Control.Monad.ST (ST, runST)

import Data.Array.ST (STArray)
import Data.Array.MArray (newArray, readArray, writeArray, getAssocs, MArray, Ix)

import Data.Sequence qualified as S

import Text.Regex.Applicative ((=~), sym, anySym, some, (<|>))

type Input = [String]
type FocalLength = Int

data Step = Step String Op deriving Show
data Op = Remove | Insert FocalLength deriving Show

data Lens = Lens String FocalLength deriving Show
type Box = S.Seq Lens
type Grid s = STArray s Int Box

hashString :: String -> Int
hashString = foldl' hash 0
  where hash current c = ((current + ord c) * 17) `mod` 256

parse :: String -> Step
parse = fromMaybe (error "no parse") . (=~ step)
  where step = Step <$> some anySym <*> op
        op = (Remove <$ sym '-') <|> (Insert . digitToInt <$> (sym '=' *> anySym))

modifyArray' :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray' a i f = readArray a i >>= write
  where write old = writeArray a i new
          where !new = f old

showBoxes :: [(Int, Box)] -> String
showBoxes = unlines . mapMaybe showBox
  where showBox (i, b) | S.null b = Nothing
                       | otherwise = Just $ "Box " <> show i <> ": " <> lenses
          where lenses = unwords . map showLens . toList $ b
                showLens (Lens label focus) = "[" <> label <> " " <> show focus <> "]"

runStep :: Grid s -> Step -> ST s ()
runStep g (Step label op) = modifyArray' g (hashString label) runOp
  where runOp box = case op of
                      Remove -> S.filter keep box
                        where keep (Lens label' _) = label /= label'
                      Insert focus -> case mapAccumL replace False box of
                        (True, box') -> box'
                        (False, box') -> box' S.|> new
                        where new = Lens label focus
                              replace seen old@(Lens label' _) | label == label' = (True, new)
                                                               | otherwise = (seen, old)

part1 :: Input -> Int
part1 = sum . map hashString

part2 :: Input -> Int
part2 input = runST $ do
  g <- newArray (0, 255) S.empty
  traverse_ (runStep g . parse) input
  sum . map boxPower . filter (not . S.null . snd) <$> getAssocs g
  where boxPower (boxNum, lenses) = (boxNum + 1) * (getSum $ S.foldMapWithIndex lensPower lenses)
          where lensPower ix (Lens _ focus) = Sum ((ix + 1) * focus)

prepare :: String -> Input
prepare = splitOn "," . filter (/= '\n')

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
