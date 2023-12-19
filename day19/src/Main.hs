{-# LANGUAGE ImpredicativeTypes #-}

module Main where

import Control.Arrow ((&&&))
import Control.Lens (over)
import Control.Monad (replicateM_)
import Data.Char (isLower)
import Data.Maybe (fromMaybe)
import Linear.V2 (V2(..), _x, _y)

import Text.Regex.Applicative ((=~), some, many, sym, psym, anySym, (<|>), asum)
import Text.Regex.Applicative.Common (decimal)

import Data.Map.Lazy qualified as M

data Part = Part { excellence, musicality, aerodynamism, shininess :: Int } deriving Show

type Label = String
data Verdict = Accept | Reject deriving (Show, Eq)
data Outcome = Final Verdict | Transition Label deriving Show
data Field = Excellence | Musicality | Aerodynamism | Shininess deriving (Show, Eq, Ord, Enum, Bounded)
data Op = Greater | Less deriving Show
data Condition = Condition Field Op Int deriving Show
data Step = Step Condition Outcome deriving Show
data Workflow = Workflow Label [Step] Outcome deriving Show
data Handler r = Handler { combine :: Condition -> r -> r -> r, final :: Verdict -> r }
type CompiledWorkflow = forall r. Handler r -> r

compile :: [Workflow] -> CompiledWorkflow
compile flows = m M.! "in"
  where m = M.fromList . map compileWorkFlow $ flows
        compileWorkFlow :: Workflow -> (Label, CompiledWorkflow)
        compileWorkFlow (Workflow label steps fallback) =
          (label, foldr compileStep (compileOutcome fallback) steps)
        compileOutcome :: Outcome -> CompiledWorkflow
        compileOutcome (Final verdict) = flip final verdict
        compileOutcome (Transition label) = m M.! label
        compileStep :: Step -> CompiledWorkflow -> CompiledWorkflow
        compileStep (Step cond outcome) next k =
          combine k cond (compileOutcome outcome k) (next k)

data Input a = Input a [Part] deriving Functor

part1 :: Input CompiledWorkflow -> Int
part1 (Input workflow parts) = sum . map score . filter ((== Accept) . go) $ parts
  where go = workflow (Handler compileCondition const)
        compileCondition :: Condition -> (Part -> Verdict) -> (Part -> Verdict) -> (Part -> Verdict)
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

type ConstraintSet = M.Map Field (V2 Int)

part2 :: Input CompiledWorkflow -> Int
part2 (Input workflow _) = sum . map countOptions . workflow $ Handler choose finalize
  where finalize Reject = []
        finalize Accept = [universe]
        universe = M.fromList [(k, V2 1 4000) | k <- [minBound ..]]
        choose (Condition f op limit) yes no = (require f op limit <$> yes) <> (forbid f op limit <$> no)
        require k op limit = M.adjust f k
          where f = case op of
                      Less -> over _y (min (limit - 1))
                      Greater -> over _x (max (limit + 1))
        forbid k op limit = M.adjust f k
          where f = case op of
                      Less -> over _x (max limit)
                      Greater -> over _y (min limit)
        countOptions = product . map fieldOptions . M.elems
        fieldOptions (V2 lo hi) = max 0 (hi - lo + 1)

prepare :: String -> Input CompiledWorkflow
prepare = fmap compile . fromMaybe (error "no parse") . (=~ input)
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
