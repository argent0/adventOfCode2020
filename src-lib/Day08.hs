{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Day08 where

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

import qualified Data.Map.Strict as Map

import Debug.Trace

import Data.Maybe (isJust)
import Control.Arrow

import Data.Array.IArray (Array)
import qualified Data.Array.IArray as IA

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either (isRight)
import Data.Maybe (mapMaybe)


data IntState = IntState
	{ _ip :: Integer
	, _acc :: Integer
	, _pacc :: Integer
	, _visited :: Set Integer } deriving Show

makeLenses ''IntState

initial :: IntState
initial = IntState 1 0 0 (Set.empty)

data Instruction = Acc Integer | Jmp Integer | Nop Integer deriving (Show, Eq, Ord)

type Input = Instruction

solver :: Array Integer Input -> Integer
solver input = head $ mapMaybe folder $ [1..maxIp]
	where
	folder :: Integer -> Maybe Integer
	folder idx = case input IA.! idx of
		Nop n -> case stopValue (input IA.// [(idx, Jmp n)]) of
			Right v -> Just v
			Left _ -> Nothing
		Acc n -> Nothing
		Jmp n -> case stopValue (input IA.// [(idx, Nop n)]) of
			Right v -> Just v
			Left _ -> Nothing

	(_, maxIp) = IA.bounds input

-- | Generates the solution from the input
stopValue :: Array Integer Input -> Either Integer Integer
stopValue input = go initial
	where
	go :: IntState -> Either Integer Integer
	go st
		| (st ^. ip) > maxIp = Right (st ^. acc)
		| (st ^. ip) `Set.member` (st ^.visited) = Left $ st ^. pacc
		| otherwise = --traceShow st $
				go $
				(over visited (Set.insert (st ^. ip))) $
				(pacc .~ (st ^. acc)) $
				case input IA.! (st ^. ip) of
					Nop _ -> over ip (+1) st
					Acc n -> (over ip (+1) $ over acc (+n) st)
					Jmp n -> over ip (+n) st
	(_, maxIp) = IA.bounds input

-- | One Rule per line
parseQ :: [String] -> Array Integer Input
parseQ input = let
		is = fmap (doer . words) input
	in IA.listArray (1, fromIntegral $ length is) is
	where
	doer :: [String] -> Input
	doer ["acc", num] = Acc (readNum num)
	doer ["jmp", num] = Jmp (readNum num)
	doer ["nop", num] = Nop (readNum num)

	readNum :: String -> Integer
	readNum ('+':num) = read num
	readNum ('-':num) = (-1) * read num

-- Apparently uou had to find the gap op the seatids
runSolution :: FilePath -> IO ()
runSolution filePath = do
	contents <- readFile filePath
	print $ solver $ parseQ $ lines contents
