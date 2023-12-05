{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (some)
import Control.Arrow ((&&&))

import Data.Map.Strict qualified as M
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative (RE, sym, string, psym, (=~))
import Text.Regex.Applicative.Common (decimal)

data Range = Range {_source, _dest, _size :: Int} deriving Show
type Mapping = IM.IntMap Range
type Ingredient = String
data Page = Page {_from, _to :: Ingredient, _ranges :: Mapping} deriving Show
type Index = M.Map Ingredient Page
data Almanac = Almanac [Int] Index deriving Show

translate :: Mapping -> Int -> Int
translate m q = maybe q go . IM.lookupLE q $ m
  where go (_, Range source dest size) | source + size > q = dest + (q - source)
                                       | otherwise = q

type Parser a = RE Char a

range :: Parser Range
range = flip Range <$> decimal <* sym ' ' <*> decimal <* sym ' ' <*> decimal

rangesToMapping :: [Range] -> Mapping
rangesToMapping = IM.fromList . map go
  where go r@(Range src _ _) = (src, r)

mapping :: Parser Mapping
mapping = rangesToMapping <$> some (range <* sym '\n')

page :: Parser Page
page = do
  sym '\n'
  src <- ingredient
  string "-to-"
  dst <- ingredient
  string " map:\n"
  m <- mapping
  pure (Page src dst m)
  where ingredient = some (psym (/= '-'))

almanac :: Parser Almanac
almanac = Almanac <$> seeds <*> (collate <$> some page)
  where seeds = string "seeds:" *> some (sym ' ' *> decimal) <* string "\n"
        collate pages = M.fromList [(src, entry) | entry@(Page src _ _) <- pages]

location :: Index -> Int -> Int
location idx = go "seed"
  where go ingredient n = maybe n handle (M.lookup ingredient idx)
          where handle (Page _ to next) = go to (translate next n)

explicit :: Mapping -> Mapping
explicit = rangesToMapping . go 0
  where go :: Int -> Mapping -> [Range]
        go n implicit = case IM.lookupLE n implicit of
          Nothing -> case IM.lookupGT n implicit of
            Nothing -> pure $ Range n n (maxBound - n + 1)
            Just (_, Range from _ _) -> Range n n (from - n) : go from implicit
          Just (_, r@(Range from _ len)) -> r : go (from + len) (IM.delete from implicit)

compose :: Mapping -> Mapping -> Mapping
compose f g = rangesToMapping (ranges 0)
  where ranges n | n < 0 = []
                 | otherwise = case IM.lookupLE n f of
                     Nothing -> error $ "Can't find " <> show n <> " in " <> show f
                     Just (_, Range src dst len) -> let offset = n - src
                                                        odst = dst + offset
                                                        olen = len - offset
                                                    in case IM.lookupLE odst g of
                                                         Nothing -> error $ "Can't find " <> show odst <> "' in " <> show g
                                                         Just (_, Range src' dst' len') ->
                                                           let offset' = odst - src'
                                                               odst' = dst' + offset'
                                                               olen' = len' - offset'
                                                               shortest = min olen olen'
                                                           in Range n odst' shortest : ranges (n + shortest)


type Input = Almanac

part1 :: Input -> Int
part1 (Almanac seeds idx) = minimum . map (location idx) $ seeds

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ almanac)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
