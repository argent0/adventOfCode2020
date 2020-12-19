{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day19 ( runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (anyChar, space, notChar, decimal, string, char, endOfLine)
import qualified Data.Attoparsec.ByteString.Char8 as DABC8
import qualified Data.Vector as V
import qualified Data.List as DL
import Debug.Trace
import Control.Arrow
import Control.Monad
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

-- Represents rules in a context free grammar
-- t: is the type for the terminal symbols
-- n: is the type for the non terminal symbols
data Rule t n
	= Concat (Rule t n) (Rule t n)
	| Union (Rule t n) (Rule t n)
	| Terminal t
	| NonTerminal n
	deriving (Show, Eq)

-- A set of rules for a context free gramar.
-- All rules have the form Nt -> x
-- So I index them by the non terminal
type RuleSet t n = Map n (Rule t n)

-- The states of the NDPA (Non deterministic push down automata)
data NDPAState = P0 | P1 deriving (Show, Eq)

-- The alphabet to be used in the NDPD's stack
data Output t n
	= StackedT t -- A stacked terminal
	| StackedNT n -- An stacked non terminal
	deriving (Show, Eq)

-- Given a rule compute what should be added to the stack,
-- This is a non-deterministic function. For example for rule:
--
-- n: A | B
--
-- There are two options so it returns [ruleAlternatives A, ruleAlternatives B]
-- 
-- For rule:
--
-- m: "a"
--
-- There is one alternative so it returns [[StackedT 'a']]
ruleAlternatives :: Rule t n -> [[Output t n]]
ruleAlternatives (NonTerminal n) = [[StackedNT n]]
ruleAlternatives (Terminal c) = [[StackedT c]]
ruleAlternatives (Union r1 r2) = concatMap ruleAlternatives [r1, r2]
ruleAlternatives (Concat r1 r2) = [concatMap (join . ruleAlternatives) [r1, r2]]

-- Compute all configurations that can be deribed from the provided one.
-- This implements the "free" NDPA for a Context Free Grammar
yield :: (Ord n, Eq t) =>
	RuleSet t n -> n ->
	NDPAState -> [t] -> [Output t n] -> [(NDPAState, ([t], [Output t n]))]

-- Transition to P1 pushing the intial symbol of the grammar. This is the only
-- valid transition for P0
yield rules initialSymbol P0 ss [] = [(P1, (ss, [StackedNT initialSymbol]))]

-- Push x for every rule: StackedNT a -> x
--
-- Is use the non-total `fromJust` becasue all non termial symbols should be on
-- the rule set.
yield rules _ P1 ss (StackedNT a:os) = (\x -> (P1, (ss, x ++ os))) <$> ruleAlternatives (fromJust $ rules !? a)

-- Out of input and with a non-empty stack means there are no more options
yield rules _ P1 [] (StackedT _:os) = []

-- Continue if the terminal in the top matches the one at the begining of the
-- stack (if it is a terminal)
yield rules _ P1 (s:ss) (StackedT g:os)
	| s == g = [(P1, (ss, os))]
	| otherwise = []

-- out of options. Notice that the string may not yet be empty.
yield _ _ P1 _ [] = []
yield _ _ _ _ _ = []

-- Checks if the string is accepted by the context free grammar defined by the
-- rules.
--
-- It uses a NDPA to compute.
runNDPA :: forall t n . (Ord n, Eq t) =>
	Int -> RuleSet t n -> n -> [t] -> Bool
runNDPA maxIter rules initialSymbol input = or $ go maxIter P0 input []
	where
	go :: Int -> NDPAState -> [t] -> [Output t n] -> [Bool]
	go 0 p i o = error "Max iterations reached"
	go n p i o = do
			(p', (ni, no)) <- yield rules initialSymbol p i o
			-- Stop when it reaches P1 with empty input and stack
			if (P1, [], []) == (p', ni, no) 
				then pure True
				else go (n - 1) p' ni no

runSolution :: FilePath -> IO ()
runSolution filePath = do
	putStrLn "**Day 19**"
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn $ "Error :" ++ err
		Right (rules, samples) -> do
			--putStrLn $ unlines $ fmap (show . (runNDPA 1000 rules &&& id)) samples
			print $ DL.foldl' (+) 0 $ fmap (fromEnum . runNDPA 1000 rules 0) samples

			-- Part 2
			-- Rules 8 and 11 are replaced with
			-- 8: 42 | 42 8
			-- 11: 42 31 | 42 11 31

			let alternateRules = DL.foldl' (\ruleSet (nonTerminal, newRule) ->
				Map.adjust (const newRule) nonTerminal ruleSet) rules
					[ (8, Union (NonTerminal 42) (Concat (NonTerminal 42) (NonTerminal 8)))
					, (11, Union (Concat (NonTerminal 42) (NonTerminal 31))
						(Concat (NonTerminal 42) (Concat (NonTerminal 11) (NonTerminal 31))))]

			--putStrLn $ unlines $ fmap (show . (runNDPA 1000 alternateRules &&& id)) $ samples
			print $ DL.foldl' (+) 0 $ fmap (fromEnum . runNDPA 1000 alternateRules 0) samples


-- Concatenation takes precedence over union
parseRules :: Parser (RuleSet Char Int)
parseRules = Map.fromList . DL.sortOn fst <$> DAB.sepBy1' parseRule endOfLine
	where
	parseRule :: Parser (Int, Rule Char Int)
	parseRule = (,) <$> (decimal <* string ": ") <*> parseUnion
	parseUnion = DAB.sepBy1' parseConcat (string " | ") >>= \case
		[r] -> pure r
		(r:rs) -> pure $! DL.foldl' Union r rs
	parseConcat = DAB.sepBy1' (DAB.choice [NonTerminal <$> decimal, parseLiteral]) (char ' ') >>= \case
		[r] -> pure r
		(r:rs) -> pure $ DL.foldl Concat r rs
	parseLiteral = Terminal <$> (char '"' *> notChar '"' <* char '"')

parseInput :: Parser (RuleSet Char Int, [String])
parseInput = 
	(,) <$>
	(parseRules <* endOfLine <* endOfLine) <*>
	DAB.sepBy1' (DAB.many1' (notChar '\n')) endOfLine

-- Right
-- 265
-- 394
