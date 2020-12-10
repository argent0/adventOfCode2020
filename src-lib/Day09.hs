{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day09 where

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

import qualified Control.Foldl as L

type Input = Integer

-- Solve part 2
solver :: Integer -> Int -> Vector Integer -> Maybe Integer
solver target depth input = finisher <$> DL.find finder window
	where
	-- Compute the sum of the max and the min
	finisher :: (Int, Int) -> Integer
	finisher (l, h) = mn + mx
		where
		-- Find the min and max using one fold
		(Just mn, Just mx) = L.fold ((,) <$> L.minimum <*> L.maximum) $ fmap (input !) [l..h]
	finder :: (Int, Int) -> Bool
	finder (l, h) = (== target) $ DL.foldl' (+) 0 $ fmap (input !) [l..h]
	-- All possible sub intervals
	window :: [(Int, Int)]
	window = concat $ filter (not . null) $ mapper <$> DL.tails [0..length input - 1]
	mapper [] = []
	mapper (x:xs) = fmap (x,) xs


-- Solve part 1
solver' :: Int -> Vector Integer -> Maybe Integer
solver' depth input =
	(\(_, p) -> input ! p) <$> DL.find finder window
	where
	finder :: ([Int], Int) -> Bool
	finder (w, v) =
		null $ findPairs (\a b -> (input ! a) + (input ! b) == (input ! v)) w
	-- All posible term locations and the location of the result
	window :: [([Int], Int)]
	window = fmap (\x -> ([x-depth..x-1], x)) [depth..Vec.length input]

-- Find all the pairs that verifies the predicate
findPairs :: forall a . (a -> a -> Bool) -> [a] -> [(a, a)]
findPairs predicate = foldr folder [] . DL.tails
	where
	folder :: [a] -> [(a, a)] -> [(a, a)]
	folder [] acc = acc
	folder (x:xs) acc = maybe acc ((: acc) . (x,)) $ DL.find (predicate x) xs

parseInput :: Parser (Vector Integer)
parseInput = Vec.fromList <$> DAB.sepBy1' DABC8.decimal endOfLine

runSolution :: FilePath -> IO ()
runSolution filePath = do
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> print err
		Right input ->
			case solver' 25 input of
				Nothing -> print "Part 1 failed"
				Just part1 -> do
					print part1
					print $ solver part1 25 input
