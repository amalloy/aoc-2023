module Main where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)
import Linear.Vector (unit)
import Linear.V2 (V2(..), _x, _y)

import Text.Regex.Applicative ((=~), some, anySym, sym, string, asum)
import Text.Regex.Applicative.Common (decimal)

type Direction = V2 Int
type Color = String
data Segment = Segment Direction Int deriving Show
data Order = Order Segment Color deriving Show
type Input = [Order]

-- Both of the below polynomial calcuiations assume the last vertex is the same as the first

-- Using "shoelace theorem" from https://en.wikipedia.org/wiki/Shoelace_formula
polynomialArea :: [V2 Int] -> Int
polynomialArea = (`div` 2) . abs . sum . (zipWith det <*> tail)
  where det (V2 y1 x1) (V2 y2 x2) = x1 * y2 - x2 * y1

-- The number of points contained within the polygon, including those on the
-- perimeter and interior points (using https://en.wikipedia.org/wiki/Pick%27s_theorem)
includedPoints :: [V2 Int] -> Int
includedPoints vs = interior + boundary
  where interior = area - (boundary `div` 2) + 1
        area = polynomialArea vs
        boundary = sum . (zipWith distance <*> tail) $ vs
        distance c1 c2 = abs . sum $ (c2 - c1)

vertices :: [Segment] -> [V2 Int]
vertices = scanl (+) 0 . map toVec
  where toVec (Segment dir len) = dir * pure len

solve :: (Order -> Segment) -> Input -> Int
solve f = includedPoints . vertices . map f

part1 :: Input -> Int
part1 = solve asWritten
  where asWritten (Order seg _c) = seg

part2 :: Input -> Int
part2 = solve colorSwap
  where colorSwap (Order _seg c) = Segment dir len
          where (mag, [d]) = splitAt 5 c
                dir = case d of
                  '0' -> unit _x
                  '1' -> unit _y
                  '2' -> - unit _x
                  '3' -> - unit _y
                  x -> error $ "unknown direction " <> show x
                len = read ("0x" <> mag)

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = some (order <* sym '\n')
        order = Order <$> segment <* sym ' ' <*> color
        color = string "(#" *> replicateM 6 anySym <* sym ')'
        segment = Segment <$> direction <* sym ' ' <*> decimal
        direction = asum [ (- unit _y) <$ sym 'U'
                         , (- unit _x) <$ sym 'L'
                         , unit _y <$ sym 'D'
                         , unit _x <$ sym 'R'
                         ]

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
