{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Day21 ( runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import qualified Data.Attoparsec.ByteString.Char8 as DABC8

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Arrow
import qualified Control.Monad as CM
import qualified Data.List as DL
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bifunctor (bimap)

import Debug.Trace

newtype Ingredient = Ingredient { extractIngredient :: Text } deriving (Show, Eq, Ord)
newtype Allergen = Allergen Text deriving (Show, Eq, Ord)

type Input = (Set Ingredient, Set Allergen)

type Translation = Map Allergen (Set Ingredient)

-- Solution for part 1
solver :: [Input] -> Integer
solver input = DL.genericLength $ filter (`Set.member` safeIngredients) $ concatMap (Set.toList <<< fst) input
	where
	-- Foreach allergen the candidate ingredients are the intersection of the
	-- ingredients lists that have the allergen.
	kvalues = concatMap mapper input
	mapper (is, as) = (, is) <$> Set.toList as

	candidateTranslation = Map.fromListWith Set.intersection kvalues

	-- All ingredients
	ingredients = DL.foldl' Set.union Set.empty $ fmap fst input

	-- Ingredients that might contain allergens
	potentialyUnsafeIngredients = DL.foldl' Set.union Set.empty $ Map.elems candidateTranslation

	safeIngredients = Set.difference ingredients potentialyUnsafeIngredients

-- Solution for part 2
-- All posible (Allergen, Ingredient) assignments that are compatible with the
-- input. Restricted to one allergen per ingredient. There can be multiple solutions.
solver2 :: [Input] -> [Text]
solver2 input = T.intercalate "," . fmap extractIngredient .
	concatMap (Set.toList . snd) . DL.sortBy (compare `on` fst) .
	concatMap Map.assocs <$> unfoldrM unfolder candidateTranslation
	where
	unfolder :: Translation -> [Maybe (Translation, Translation)]
	unfolder translation
		| Map.null translation = [Nothing]
		| otherwise = let
				(solved, unsolved) = Map.partition ((== 1) . Set.size) translation
				solvedIngredients = DL.foldl' Set.union Set.empty $ Map.elems solved
			in if Map.null solved
				then
					-- Some allergens can be present in more than one
					-- ingredient.
					-- At this stage unsolved (shouldn't be null)
					case Map.assocs unsolved of
						((a, is) : as) -> do
							ingredient <- Set.toList is
							let candidate = Map.fromList [(a, Set.singleton ingredient)]
							let restUnsolved = Map.map (Set.filter (/= ingredient)) unsolved
							-- Continue only if all allergens have at least one
							-- candidate ingredient.
							CM.guard $ not $ any Set.null restUnsolved
							pure $ Just (candidate, restUnsolved)
				else do
					CM.guard $ Map.size solved == Set.size solvedIngredients
					-- There is one allergen per solved ingredient
					pure $ Just
						( solved
						, Map.map (Set.filter (not . (`Set.member` solvedIngredients))) unsolved )
	kvalues = concatMap mapper input
	mapper (is, as) = (, is) <$> Set.toList as

	candidateTranslation = Map.fromListWith Set.intersection kvalues

parseInput :: Parser [Input]
parseInput = DAB.many1' (
	fmap ((Set.fromList . fmap (Ingredient . T.pack)) *** (Set.fromList . fmap (Allergen . T.pack)) ) . (,) <$>
	DAB.sepBy1'
		(DAB.many1' DABC8.letter_ascii)
		DABC8.space <*>
	( (DABC8.string " (contains " DAB.<?> "missing contains") *>
	DAB.sepBy'
		(DAB.many1' DABC8.letter_ascii)
		(DABC8.string ", " DAB.<?> "wrong separator?")
	<* DABC8.char ')' <* DABC8.endOfLine ))

runSolution :: FilePath -> IO ()
runSolution filePath = do
	putStrLn "**Day 21**"
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn err
		Right input ->
			putStrLn $ T.unpack $ T.unlines $ solver2 input

-- | unfoldr in a monad.
--
-- This is missing from Data.List
unfoldrM :: Monad m => (a -> m (Maybe (b, a))) -> a -> m [b]
unfoldrM f a = f a >>= \case
	Nothing -> pure []
	Just (b, a') -> (b :) <$> unfoldrM f a'
