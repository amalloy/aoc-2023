{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

import Data.Map.Strict qualified as M

data Direction = North | East | South | West deriving (Show, Eq, Ord, Enum)
data Pipe = Horiz | Vert | F | J | L | Seven deriving (Show, Enum)

data Coord = Coord {_y, _x :: Int} deriving (Show, Eq, Ord)
instance Semigroup Coord where
  Coord y1 x1 <> Coord y2 x2 = Coord (y1 + y2) (x1 + x2)
instance Monoid Coord where
  mempty = Coord 0 0

data Feature = Empty | Start | Pipe Pipe deriving Show

directions :: [Direction]
directions = [North ..]
pipeShapes :: [Pipe]
pipeShapes = [Horiz ..]

unit :: Direction -> Coord
unit dir = case dir of
  North -> Coord (-1) 0
  South -> Coord 1 0
  East -> Coord 0 1
  West -> Coord 0 (-1)

invert :: Direction -> Direction
invert = toEnum . (`mod` 4) . (+ 2) . fromEnum

feature :: Char -> Feature
feature c = fromMaybe (error $ "no such pipe: " <> pure c) . (`lookup` pipeNames) $ c
  where pipeNames = (('S', Start) : ('.', Empty) : zip "-|FJL7" (map Pipe pipeShapes))

endpoints :: Pipe -> (Direction, Direction)
endpoints p = case p of
  Horiz -> (East, West)
  Vert -> (North, South)
  F -> (South, East)
  J -> (North, West)
  L -> (North, East)
  Seven -> (South, West)

mkPipe :: (Direction, Direction) -> Pipe
mkPipe p = head [q | q <- pipeShapes, let e = endpoints q, e == p || e == p']
  where p' = swap p

transit :: Pipe -> Direction -> Direction
transit = connect . endpoints
  where connect (a, b) dir | dir == invert a = b
                           | otherwise = a


type Input = (Coord, M.Map Coord Feature)

part1 :: Input -> ()
part1 = const ()

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = orient . map (map feature) . lines
  where orient :: [[Feature]] -> Input
        orient grid = let initialGrid = M.fromList $ do
                            (y, row) <- zip [0..] grid
                            (x, p) <- zip [0..] row
                            pure (Coord y x, p)
                          inferPipeType c = case filter (pointsAt c) directions of
                            [i, j] -> mkPipe (i, j)
                            xs -> error $ "Origin connected to by " <> show xs
                          pointsAt o dir = case initialGrid M.! (o <> unit dir) of
                            Pipe p -> let (a, b) = endpoints p
                                      in a == dir' || b == dir'
                            x -> error $ "surprised by: " <> show x
                            where dir' = invert dir
                          go :: Maybe Coord -> Coord -> Feature -> (Maybe Coord, Feature)
                          go _o c Start = (Just c, Pipe $ inferPipeType c)
                          go o _c f = (o, f)
                      in case M.mapAccumWithKey go Nothing initialGrid of
                           (Nothing, _) -> error "Couldn't find origin"
                           (Just o, m) -> (o, m)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
