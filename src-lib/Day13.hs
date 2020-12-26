{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day13 where

import Control.Lens
import Control.Lens.TH

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser, (<?>))
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
import Data.Maybe (isJust, mapMaybe, catMaybes)
import Data.Bool (bool)

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec

import Data.Function (on)

import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State

import qualified Control.Monad as CM

import qualified Control.Foldl as L

import Data.Complex
import Data.Functor
import Data.Bifunctor (bimap)

-- Each input is the id of a bus line. Nothing means that the line, whatever its
-- id was, is out of service.
type Input = Maybe BusId

newtype BusId = BusId { getBusId :: Integer } deriving Show

-- Parases an unsigned decimal number
parseBusId :: Parser BusId
parseBusId = (BusId <$> decimal) <?> "BusId"

newtype Timestamp = Timestamp { getTimestamp :: Integer } deriving Show

parseTimestamp :: Parser Timestamp
parseTimestamp = (Timestamp <$> decimal) <?> "Timestamp"

-- Solve part 1
--
-- forall n m : int,
--
-- 	start + n = bus_id * m
--
-- n is the waiting time for the m-th departure of `bus_id`. This can be
-- rearranged as:
--
-- 	(1)	n = bus_id * m - start
--
-- `start` can be rewriten in terms of `bus_id`:
--
-- 	start = bus_id * q + r
--
-- where
--
-- 	q = start / bus_id
-- 	r = start % bus_id
--
-- with
--
-- 	r < bus_id
--
-- replacing in (1)
--
-- 	n = bus_id * (m - q) - r
--
-- So the smallest positive n happens when (m - q) == 1 and is thus equal to
--
-- 	n_min = bus_id - r
--
-- Returns Norhing in case there are no bus lines.
solver :: Timestamp -> [Input] -> Maybe Integer
solver start input = do
	-- Find the bus line with the earliest departure time.
	(busId, departureTime) <-
		L.fold ( L.minimumBy (compare `on` snd) ) $
		(id &&& mapper) <$> catMaybes input

	pure (getBusId busId * departureTime)
	where
	-- Compute the earliest departure time after `start` for a bus line.
	mapper :: BusId -> Integer
	mapper bid = getBusId bid - ( getTimestamp start `mod` getBusId bid )

-- Write equations for part 2
--
-- For each bus_id, n_i is the departure time for the m_i-th departure
--
-- n_1 = bus_id_1 * m_1
-- n_2 = bus_id_2 * m_2
-- ...
-- n_i = bus_id_i * m_i
--
-- Those departure times have to be consecutive so:
--
-- n_2 = n_1 + (ord_2 - ord_1)
-- ...
-- n_(i+1) = n_i + (ord_(i+1) - ord_i)
--
-- where ord_i is the order of the bus_id on the list. With ord_(i+1) > ord_i.
--
-- Applying this constrain we get:
--
-- n_1 = bus_id_1 * m_1
-- n_1 + (ord_2 - ord_1) = bus_id_2 * m_2
-- n_2 + (ord_3 - ord_2) = bus_id_3 * m_3
-- ...
-- n_i + (ord_(i+1) - ord_i) = bus_id_(i+1) * m_(i+1)
--
-- Leaving only n_1 we get:
--
-- n_1 = bus_id_1 * m_1
-- n_1 + (ord_2 - ord_1) = bus_id_2 * m_2
-- (n_1 + (ord_2 - ord_1)) + (ord_3 - ord_2) = bus_id_3 * m_3
-- ...
--
-- The last equation becomes
--
-- n_1 + (ord_3 - ord_1) = bus_id_3 * m_3
--
-- So all the equations become
--
-- n_1 = bus_id_1 * m_1
-- n_1 + (ord_2 - ord_1) = bus_id_2 * m_2
-- n_1 + (ord_3 - ord_1) = bus_id_3 * m_3
-- ...
-- n_1 + (ord_i - ord_1) = bus_id_i * m_i
--
-- Next I rewrite them like:
--
-- n_1 - bus_id_1 * m_1 = 0
-- n_1 + (ord_2 - ord_1) - bus_id_2 * m_2 = 0
-- ...
-- n_1 + (ord_i - ord_1) - bus_id_i * m_i = 0
--
-- Finally I replace n_1 in all the equations
--
-- bus_id_1 * m_1 + (ord_i - ord_1) - bus_id_i * m_i = 0
--
-- This equations can be stored using a 4-tuple
--
--	(bus_id_1, bus_id_i, (ord_i - ord_1), ord_i)
--
-- This functions just creates those 4-tuples to be further processed.
solver' input = case catMaybes (zipWith (\x -> fmap (x,)) [1..] input) of
	( (ord1, busId1) : rest) ->
		(busId1, busId1, 0, ord1) : fmap mapper rest
		where
		mapper (ordi, busIdi) = (busId1, busIdi, ordi - ord1, ordi)

-- Sample input
--
-- 939
-- 7,13,x,x,59,x,31,19
--
-- The x's represent lines that are out of service.
parseInput :: Parser (Timestamp, [Input])
parseInput =
	( (,) <$>
		(parseTimestamp <* endOfLine) <*>
		(DAB.choice
			[ Just <$> parseBusId
			, char 'x' $> Nothing ] `DAB.sepBy1'` char ',') ) <* endOfLine


runSolution :: FilePath -> IO ()
runSolution filePath = do
	putStrLn "**Day 13**"
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn $ "Error :" ++ err
		Right (start, input) -> do

			putStrLn "Part 1"
			print $ solver start input

			putStrLn "\nPart 2"
			mapM_ (putStrLn . printEq) $ solver' input
	where
	-- Prints an equation to be solved.
	printEq (flid, bid, delta, ordi) =
		show (getBusId flid) ++ " * x[1] - " ++
		show (getBusId bid) ++ " * x[" ++ show ordi ++ "] + " ++
		show delta

-- Wrong
-- 1258

-- Part 1:
-- Answer = Just 410
