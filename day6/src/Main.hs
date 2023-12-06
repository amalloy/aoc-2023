{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative (RE, string, some, sym, (=~))
import Text.Regex.Applicative.Common (decimal)

newtype Millisecond = Ms {getMs :: Int} deriving Show
newtype Millimeter = Mm {getMm :: Int} deriving Show

data Record = Record {time :: Millisecond, distance :: Millimeter} deriving Show

type Input = [Record]

input :: RE Char Input
input = zipWith Record <$> row "Time" Ms <*> row "Distance" Mm
  where row label f = string label *> sym ':' *> some (some (sym ' ') *> (f <$> decimal)) <* sym '\n'

waysToBeatRecord :: Record -> Int
waysToBeatRecord (Record (Ms ms) (Mm mm)) =
  ms - 2 * (floor recordTime + 1) + 1
  where recordTime :: Double
        recordTime = (k - sqrt (k*k - 4*d)) / 2
          where k = fromIntegral ms
                d = fromIntegral mm

part1 :: Input -> Int
part1 = product . map waysToBeatRecord

part2 :: Input -> Int
part2 records = waysToBeatRecord record
  where record = Record (Ms (kern times)) (Mm (kern distances))
        times = map (getMs . time) records
        distances = map (getMm . distance) records
        kern = read . (>>= show)

prepare :: String -> Input
prepare = fromMaybe (error "No parse") . (=~ input)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
