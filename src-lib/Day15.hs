{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Day15 where

import Control.Lens
import Control.Lens.TH

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import qualified Data.Attoparsec.ByteString.Char8 as DABC8
import Data.Attoparsec.ByteString.Char8 (decimal, inClass, digit, endOfLine, char, anyChar, letter_ascii, notChar)

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
import Data.Maybe (mapMaybe, catMaybes)
import Data.Bool (bool)

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec

import Data.Function (on)

import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State

import qualified Control.Monad as CM

import qualified Control.Foldl as L

import Data.Bifunctor (bimap)

import Data.Complex
import Data.Functor

import Data.Word
import Data.Bits
import Data.Semigroup
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as DLNE

type Input = Integer

solver input fTurn = State.evalState (code 6 (fromIntegral $ length input + 1)) $ -- turns start at length input + 1
	Map.fromList $ zip input $ fmap (,Nothing) [1..] --Insert the seed input with their order
	where
	-- Monadic loop. Maybe avoid custom recursion
	-- It returns the fTurn-th value
	code :: Integer -> Integer -> State (Map Integer (Integer, Maybe Integer)) Integer
	code prev turn
		| turn > fTurn = pure prev
		| otherwise =
			-- Check is the previous value is repeated
			State.get >>= \m -> case Map.lookup prev m of
				Just (_, Nothing) -> do --not repeated
					State.modify (Map.insertWith inserter 0 (turn, Nothing))
					code 0 (turn + 1)
				Just (l, Just ll) -> do -- repeated
					State.modify (Map.insertWith inserter (l-ll) (turn, Nothing))
					code (l - ll) (turn + 1)

	-- Update the map keeping the previous position
	-- DONE: Use a tupple (Integer, Maybe Integer)
	-- DONE: avoid error (removed safety check)
	inserter :: (Integer, Maybe Integer) -> (Integer, Maybe Integer) -> (Integer, Maybe Integer)
	inserter (!new, Nothing) (!prev, _) = (new, Just prev)
		-- | new > prev =  (new, Just prev)
		-- | otherwise = error $ show (prev, new)

runSolution :: FilePath -> IO ()
runSolution _ = do
	putStrLn "**Day 15**"
	print $ solver input 30000000
	where
	input :: [Input]
	--input = [0,3,6]
	input = [0,13,1,16,6,17]
