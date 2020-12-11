{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day10 where

import Control.Lens
import Control.Lens.TH

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import qualified Data.Attoparsec.ByteString.Char8 as DABC8
import Data.Attoparsec.ByteString.Char8 (inClass, digit, endOfLine, char, anyChar, letter_ascii)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Data.List as DL
import qualified Data.List.Split as DLS

import Debug.Trace

import Control.Arrow

import Data.Array.IArray (Array)
import qualified Data.Array.IArray as IA

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either (isRight)
import Data.Maybe (mapMaybe)
import Data.Bool (bool)

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec

import Data.Function (on)

import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State

import qualified Control.Monad as CM

import qualified Control.Foldl as L

type Input = Integer


type Mon = State (Map [Integer] Integer)

-- Memoized working function. This is kind of brute force.
-- But doesn't exploit the particular input.
worker :: [Integer] -> Mon Integer
worker input = do
	m <- State.get
	case Map.lookup input m of
		Just v -> pure v -- Hit a cached input
		Nothing -> do
			-- Compute the result for the input
			a <- CM.foldM folder 1 $ DL.tails input
			-- Save result to cache
			State.modify (Map.insert input a)
			pure a
	where
	folder :: Integer  -> [Integer] -> Mon Integer
	folder n [] = pure n
	folder n (a:as) = case suit of
		-- should be the End of input
		[] -> pure n
		-- There is only one option
		[_] -> pure n
		-- There are two alternatives, so we add the effect of dropping the
		-- first
		[_, _] -> (n +) <$> worker (drop 1 as)
		-- There are three alternatives
		-- Dropping the first
		-- Dropping droping the first and the second
		-- So we add those cases
		[_, _, _] -> (n +) <$> ( (+) <$> worker (drop 1 as) <*> worker (drop 2 as) )
		where
		suit = takeWhile (<= a + 3) as

-- Solve part 2
solver' input = State.evalState (worker ordered) Map.empty
	where

	ordered =  (++[adapter]) $ (0:) $ DL.sort $ Vec.toList input
	adapter = Vec.maximum input + 3

-- This solution exploits particularities of the input outside the problem
-- description
solver'' input =
	L.fold L.product $ fmap (mapper . length) $ filter ((==1) . head) $ DL.group diffs
	where
	-- How many ways are there to sum 4 using 1,2 and 3
	mapper 4 = 7
	mapper 3 = 4
	mapper 2 = 2
	mapper 1 = 1
	diffs =  zipWith (-)  (tail ordered) ordered
	ordered =  (++[adapter]) $ (0:) $ DL.sort $ Vec.toList input
	adapter = Vec.maximum input + 3

-- Could solve part 1
solver input = traceShow diffs $
	uncurry (*) $ L.fold (L.Fold (\(c1, c3) d -> case d of
		1 -> (c1 + 1, c3)
		3 -> (c1, c3 + 1)
		_ -> (c1, c3))
		 (0 :: Integer, 0 :: Integer) id) diffs
	where
	diffs =  zipWith (-)  (tail ordered) ordered
	ordered =  (++[adapter]) $ (0:) $ DL.sort $ Vec.toList input
	adapter = Vec.maximum input + 3

parseQ :: [String] -> Vector Integer
parseQ = Vec.fromList . fmap read

runSolution :: FilePath -> IO ()
runSolution filePath = do
	contents <- readFile filePath
	print $ solver $ parseQ $ lines contents
	print $ solver'' $ parseQ $ lines contents
	--case solver' 25 $ parseQ $ lines contents of
	--	Nothing -> print "Part 1 failed"
	--	Just part1 -> do
	--		print part1
	--		print $ solver part1 25 $ parseQ $ lines contents
