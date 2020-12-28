{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Day16 ( runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (string, notChar, decimal, string, char, endOfLine)
import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as DLNE
import qualified Data.Foldable as DF
import qualified Control.Foldl as L
import Control.Lens
import Data.Function (on)
import Control.Arrow

-- Remmaining issues:
--
-- All tickets should have the same number of fields.
-- There should be as many rules are there are fields.

type Ticket = NonEmpty Int
type Input = NonEmpty Ticket
data Rule = Rule String (Int, Int) (Int, Int) deriving Show

ruleName :: Lens' Rule String
ruleName f (Rule n a b) = fmap (\nn -> Rule nn a b) (f n)

-- | Solve part 1.
--
-- Sum all invalid values over all tickets
solver :: NonEmpty Rule -> Input -> Int
solver rules = L.fold (L.premap (L.fold L.sum) L.sum) . fmap (extractInvalid rules)

-- | Solver part 2
--
-- The list araises from the fact that there could be many possible valid
-- assignments (Rule -> Column)
solver2 :: NonEmpty Rule -> Input -> Ticket -> [Int]
solver2 rules input ticket = case validTickes of
	[] -> []
	(t : ts) ->
		-- Map over all posible valid assignments (Rule -> Column)
		fmap (
			-- Compute the product of the extracted fields
			L.fold L.product .
			-- Extract the fields of `ticket`
			fmap fst .
			-- Only keep the fields that start with "departure"
			filter (DL.isPrefixOf "departure" . (^. ruleName) . snd) .
			-- Zip the `ticket` with the sorted rules
			zip (DLNE.toList ticket) .
			-- Sort the rules by field order
			fmap fst . DL.sortBy (compare `on` snd)
		) $
		-- Unfold the list of (Rule -> Column) matchings. All valid alternatives.
		unfoldrM unfold $
		-- Sort the rules, putting the one with the least posible matching columns
		-- first
		DL.sortBy (compare `on` (length . snd)) $
		-- Group rules together
		fmap mapper $ DL.groupBy ((==) `on` ( (^. ruleName) . fst)) $
		-- Only keep the rule and the matching column
		fmap (fst &&& fst . snd) $
		-- Only keep the (Rule, Column) pairs where all column elements satisfy the
		-- rule.
		DLNE.filter (snd . snd) $
		-- Compute if a Rule matches a column for all combinations of rules and
		-- columns.
		(\r (cn, c) -> (r, (cn, all (matchRule r) c))) <$> rules <*> DLNE.zip (1 :| [2..]) cols

		where
		unfold :: [(Rule, [Int])] -> [Maybe ((Rule, Int), [(Rule, [Int])])]
		unfold [] = [Nothing]
		-- It can have multiple alternatives
		--
		-- This assigns rule r to each column c in cs.
		-- Then remove the option c from all the remaining rules.
		--
		-- If there are no options for a rule then there is no valid alternative
		unfold ((r, []) : rest) = []
		unfold ((r, cs@(_ : _)) : rest) = fmap (\c -> Just
			( (r, c)
			, DL.sortBy (compare `on` (length . snd)) $ fmap (second $ filter (/=c))  rest))
			cs
		mapper :: [(Rule, Int)] -> (Rule, [Int])
		mapper ((rule, h):rest) = (rule, h: fmap snd rest)
		cols = columns (t :| ts)

	where
	validTickes = DLNE.filter (isValid rules) input

-- | Whether a value staisfies a rule
matchRule :: Rule -> Int -> Bool
matchRule (Rule _ (l1, h1) (l2, h2)) n =
	(l1 <= n) && (n <= h1) ||
	(l2 <= n) && (n <= h2)

-- | Extract the values of a ticket that don't satisfiy any rule
extractInvalid :: NonEmpty Rule -> Ticket -> [Int]
extractInvalid rules = DLNE.filter (not . matchAnyRule)
	where
	matchAnyRule v = any (`matchRule` v) rules

isValid :: NonEmpty Rule -> Ticket -> Bool
isValid rules = null . extractInvalid rules

-- | Put each field into its own column
columns :: Input -> NonEmpty (NonEmpty Int)
columns (t :| ts) = DLNE.reverse <$> DF.foldl' (flip folder) (fmap (:| []) t) ts
	where
	folder :: NonEmpty Int -> NonEmpty (NonEmpty Int) -> NonEmpty (NonEmpty Int)
	folder = DLNE.zipWith DLNE.cons

parseTicket :: Parser Ticket
parseTicket = 
	DLNE.fromList <$> ( decimal `DAB.sepBy1'` char ',' ) <* endOfLine

parseRule :: Parser Rule
parseRule = Rule <$> DAB.many1' (notChar ':') <*>
	( string ": " *> parseRange ) <*>
	( string " or " *> parseRange)
	<* endOfLine
	where
	parseRange = (,) <$> decimal <*> (char '-' *> decimal)

parseInput :: Parser (NonEmpty Rule, Ticket, Input)
parseInput = do
	rules <- DLNE.fromList <$> (DAB.many1' parseRule DAB.<?> "Rules")
	_ <- endOfLine
	_ <- string "your ticket:" <* endOfLine
	yt <- parseTicket
	_ <- endOfLine
	_ <- string "nearby tickets:" <* endOfLine
	nbt <- DLNE.fromList <$> DAB.many1' parseTicket
	return (rules, yt, nbt)

runSolution :: FilePath -> IO ()
runSolution filePath = do
	putStrLn "**Day 16**"
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn $ "Error :" ++ err
		Right (rules, yourTicket, input) -> do
			print $ solver rules input
			print $ solver2 rules input yourTicket

-- | unfoldr in a monad.
--
-- This is missing from Data.List
unfoldrM :: Monad m => (a -> m (Maybe (b, a))) -> a -> m [b]
unfoldrM f a = f a >>= \case
	Nothing -> pure []
	Just (b, a') -> (b :) <$> unfoldrM f a'
