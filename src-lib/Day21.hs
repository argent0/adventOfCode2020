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

newtype Ingredient = Ingredient Text deriving (Show, Eq, Ord)
newtype Allergen = Allergen Text deriving (Show, Eq, Ord)

type Input = (Set Ingredient, Set Allergen)

type Translation = Map Allergen (Set Ingredient)

solver :: [Input] -> [Translation]
solver = fmap (DL.foldl1 Map.union) <<< unfoldrM solutionUnfolder <<<
	fmap (DL.foldl' folder ([], Set.empty)) <<<
	DL.sortBy (flip compare `on` length) <<<
	DL.groupBy ((==) `on` snd) <<< DL.sortBy (compare `on` snd)
	where
	folder ([], as) (is, as')
		| Set.null as = ([is], as')

	folder (acc@(_ : _), as) (is, as')
		| as == as' = (is : acc, as)
		| otherwise = error "Group error"

solutionUnfolder :: [([Set Ingredient], Set Allergen)] ->
	[ Maybe (Translation, [([Set Ingredient], Set Allergen)]) ]
solutionUnfolder [] = [Nothing]
solutionUnfolder (((is, as) : rest)) =
	let result = fmap (
		\trans -> let
			filtered = filter (not . Set.null . snd) $ fmap
				(\(ris, ras) ->
					( fmap (Set.filter (`notElem` Map.keys trans)) ris
					, Set.filter (`notElem` Map.elems trans) ras) )
				rest
			in
				( trans
				, concat $ DL.sortBy (flip compare `on` length) $
					DL.groupBy ((==) `on` snd) $ DL.sortBy (flip compare `on` snd) filtered)
			) $ translations (repeated, as)
	in Just <$> result
		-- (filter
		-- 	(\(_, next) -> DL.any (\(ris, ras) -> DL.any ((>= length ras) . length)  ris)  next)
		-- 	result)
	where
	repeated = case is of
		[] -> Set.empty
		( i : is') -> DL.foldl' Set.intersection i is'

--solver input = CM.foldM solutionFolder Map.empty $ DL.sortBy (compare `on` (length . fst)) input

-- Given a current translation and a new line of input return all valid
-- subsequent translations.
--
--solutionFolder :: Translation -> Input -> [Translation]
--solutionFolder translation (ingredients, allergens) = do
--	let (knownAllergens, unknownAllergens) =
--	 	DL.partition (`elem` Map.elems translation) allergens
--
--	-- all ingredients corresponding to the known allergens should be present in
--	-- the ingredients list.
--	CM.guard (DL.all ((`elem` ingredients) . fst) (filter ((`elem` knownAllergens) . snd)  $ Map.assocs translation))
--
--	let (knownIngredients, unknownIngredients) =
--		DL.partition (`elem` Map.keys translation) ingredients
--
--	CM.guard (length unknownAllergens <= length unknownIngredients)
--
--	trans <- translations (unknownIngredients, unknownAllergens)
--	pure $ Map.union translation trans

-- Return all possible subsequences of length n from a list. Orther matters.
--
-- [1, 2] /= [2, 1]
subsequences :: Eq a =>  Int -> [a] -> [[a]]
subsequences n []
	| n < 0 = error "negative n"
	| n == 0 = [[]]
	| otherwise = []
subsequences n (a : as)
	| n < 0 = error "negative n"
	| n == 0 = [[]]
	| otherwise = do
		h <- a : as
		
		(h :) <$> subsequences (n - 1) (filter (/= h) (a : as))

-- Given a list of ingredients and allergens return all possible translations of
-- ingredients into allergens.
--
-- Eg: ("a", "b", "c"), ("1", "2", "3")
--
-- a : 1
-- b : 2
--
-- a : 2
-- b : 1
--
-- ...
translations :: Input -> [Translation]
translations (ingredients, alergens) = do
	ing <- subsequences (length alergens) $ Set.toList ingredients

	pure $ Map.fromList $ zip ing (Set.toList alergens)

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
			putStrLn $ unlines $ fmap show $ solver input

	-- print $ solver $ parseQ $ lines contents

-- | unfoldr in a monad.
--
-- This is missing from Data.List
unfoldrM :: Monad m => (a -> m (Maybe (b, a))) -> a -> m [b]
unfoldrM f a = f a >>= \case
	Nothing -> pure []
	Just (b, a') -> (b :) <$> unfoldrM f a'
