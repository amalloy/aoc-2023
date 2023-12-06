{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative (RE, string, some, sym, (=~))
import Text.Regex.Applicative.Common (decimal)

newtype Millisecond = Ms Int deriving Show
newtype Millimeter = Mm Int deriving Show

data Record = Record Millisecond Millimeter deriving Show

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

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe (error "No parse") . (=~ input)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
