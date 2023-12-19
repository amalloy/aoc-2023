module Main where

import Control.Arrow ((&&&))
import Control.Monad (replicateM_)
import Data.Char (isLower)
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative ((=~), some, many, sym, psym, anySym, (<|>), asum)
import Text.Regex.Applicative.Common (decimal)

import Data.Map.Lazy qualified as M

data Part = Part { excellence, musicality, aerodynamism, shininess :: Int } deriving Show

type Label = String
data Verdict = Accept | Reject deriving (Show, Eq)
data Outcome = Final Verdict | Transition Label deriving Show
data Field = Excellence | Musicality | Aerodynamism | Shininess deriving (Show, Enum, Bounded)
data Op = Greater | Less deriving Show
data Condition = Condition Field Op Int deriving Show
data Step = Step Condition Outcome deriving Show
data Workflow = Workflow Label [Step] Outcome deriving Show
type CompiledWorkflow = Part -> Verdict

compile :: [Workflow] -> CompiledWorkflow
compile flows = m M.! "in"
  where m = M.fromList . map compileWorkFlow $ flows
        compileWorkFlow :: Workflow -> (Label, CompiledWorkflow)
        compileWorkFlow (Workflow label steps fallback) =
          (label, foldr compileStep (compileOutcome fallback) steps)
        compileOutcome :: Outcome -> CompiledWorkflow
        compileOutcome (Final verdict) = const verdict
        compileOutcome (Transition label) = m M.! label
        compileStep :: Step -> CompiledWorkflow -> CompiledWorkflow
        compileStep (Step cond outcome) next =
          compileCondition cond (compileOutcome outcome) next
        compileCondition :: Condition -> CompiledWorkflow -> CompiledWorkflow -> CompiledWorkflow
        compileCondition (Condition field op threshold) met failed = run
          where run p | acceptable p = met p
                      | otherwise = failed p
                acceptable p = get p ?= threshold
                get = compileField field
                (?=) = compileOp op
        compileField :: Field -> Part -> Int
        compileField f = case f of
          Excellence -> excellence
          Musicality -> musicality
          Aerodynamism -> aerodynamism
          Shininess -> shininess
        compileOp :: Op -> Int -> Int -> Bool
        compileOp Greater = (>)
        compileOp Less = (<)

score :: Part -> Int
score (Part x m a s) = x + m + a + s

data Input = Input [Workflow] [Part] deriving Show

part1 :: Input -> Int
part1 (Input workflows parts) = sum . map score . filter ((== Accept) . run) $ parts
  where run = compile workflows

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = Input <$> some (workflow <* newline) <* newline <*> some (part <* newline)
        newline = sym '\n'
        p `sepBy` sep = ((:) <$> p <*> many (sep *> p)) <|> pure []
        part = Part <$> attr <*> attr <*> attr <*> (attr <* sym '}')
        attr = replicateM_ 3 anySym *> decimal
        workflow = Workflow <$> label <* sym '{' <*> (step `sepBy` (sym ',')) <*> (sym ',' *> outcome) <* sym '}'
        label = some (psym isLower)
        outcome = (Final <$> verdict) <|> (Transition <$> label)
        verdict = (Accept <$ sym 'A') <|> (Reject <$ sym 'R')
        step = Step <$> condition <* sym ':' <*> outcome
        condition = Condition <$> field <*> op <*> decimal
        op = (Greater <$ sym '>') <|> (Less <$ sym '<')
        field = asum (zipWith go "xmas" [minBound ..])
          where go c f = f <$ sym c

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
