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

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec

import Data.Function (on)

import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State

import qualified Control.Monad as CM

--import qualified Control.Foldl as L

type Input = Integer


sum' :: Num a => [a] -> a
sum' = DL.foldl' (+) 0

type Mon = State (Map [Integer] Integer)

-- Memoized working function. This is kind of brute force.
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
		-- Dropping the first (this includes optionally droping de second)
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

-- Could solve part 1
-- solver' input = traceShow ordered $
-- 	DL.foldl' folder (1, [], []) $ reverse ordered
-- 	where
-- 	folder :: (Integer, [Integer], [Integer]) -> Integer -> (Integer, [Integer], [Integer])
-- 	folder (1, [], []) last = (1, [last], [])
-- 
-- 	folder (n, abl, last) new = traceShow (n, new, suit, stillSuit)
-- 		--(n * fromIntegral (length suit - fromEnum stillSuit), new:abl, suit)
-- 		$ if stillSuit
-- 			then (n * 3, new:abl, suit)
-- 			else (n * fromIntegral (length suit - fromEnum stillSuit), new:abl, suit)
-- 		where
-- 		suit = takeWhile (<= new + 3) abl
-- 		stillSuit = not $ null $ drop 1 $ takeWhile (<= new + 3) last
-- 
-- 	diffs =  zipWith (-)  (tail ordered) ordered
-- 	ordered =  (++[adapter]) $ (0:) $ DL.sort $ Vec.toList input
-- 	adapter = Vec.maximum input + 3

parseQ :: [String] -> Vector Integer
parseQ = Vec.fromList . fmap read

runSolution :: FilePath -> IO ()
runSolution filePath = do
	contents <- readFile filePath
	print $ solver' $ parseQ $ lines contents
	--case solver' 25 $ parseQ $ lines contents of
	--	Nothing -> print "Part 1 failed"
	--	Just part1 -> do
	--		print part1
	--		print $ solver part1 25 $ parseQ $ lines contents
