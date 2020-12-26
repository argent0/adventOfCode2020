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
import Data.Maybe (mapMaybe, catMaybes)
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

solver :: Timestamp -> [Input] -> Maybe Integer
solver start input = do
	-- Find the bus line with the earliest departure time.
	(busId, departureTime) <-
		L.fold ( L.minimumBy (compare `on` snd) ) $
		-- Find the earliest departure time after `start` for each bus line.
		mapMaybe (>>= mapper) input

	pure (getBusId busId * (departureTime - getTimestamp start))
	where
	-- The Maybe arises from the find function.
	mapper :: BusId -> Maybe (BusId, Integer)
	mapper bid@(BusId n) = (bid,) <$> DL.find (>= getTimestamp start) (iterate (+n) 0)

solver' input = case input of
	(Just firstLine : rest) ->
		fmap mapper $
			mapMaybe (uncurry ((<$>) . (,))) $ zip [1..] rest
		where
		mapper (order, lineId) = (lineId, firstLine, order)

--gen x = 13*(x - 23602961251) + 306838496270
gen x = 13*x + 306838496270

doer = DL.find finder $ codom gen
	where
	finder c = DL.all (==0) $ traceShowId $ fmap (\(p, o) -> (c*37 + o) `mod` p) known

--candidate =  case DL.sortBy (compare `on` ($ 0)) offset of
--		(b:rest) -> traceShow (take 100 $ codom b) $ finder (fmap codom rest) $ codom b
--	where
--	finder :: [[Integer]] -> [Integer] -> [Integer]
--	finder tops (c:cs) =
--		let new = fmap (dropWhile (<c)) $ tops
--		in if DL.all (==c) (fmap head new)
--			then fmap head new
--			else --traceShow (fmap (\x -> x - c) $ fmap head new) $
--				finder new cs
--	--finder :: [Integer -> Integer] -> Integer -> Bool
--	--finder rest x = traceShow x $ DL.all(==x) $ traceShowId $ mapMaybe (DL.find (>=x) . codom) rest
--	offset :: [Integer -> Integer]
--	offset = zipWith (\c g -> g . (+c)) cuts generators
--	cuts = fmap fst $ mapMaybe (findCut 0) generators
--	findCut v f = DL.find ((>=v) . snd) $ fmap (id &&& f) [0..]
--	--DL.find (DL.all (==0) . traceShowId . snd) $ fmap (\x -> (x, fmap ($ x) generators')) [1..]

codom f = fmap f [0..]
findCut v f = DL.find ((>=v) . snd) $ fmap (id &&& f) [0..]

known = [ ( 41   , 27 ),( 433  , 37 ),( 23   , 45 ),( 17   , 54 ),
	  ( 19   , 56),( 29   , 66 ),( 593  , 68 ),( 13   , 81 )]
--solveIDE :: Integer -> Integer -
--solveIDE a b c 
--	| c `mod` thisgcd == 0 = traceShow (a, b, c) $ Just (thisgcd, c)
--	| otherwise = Nothing
--	where
--	thisgcd = gcd a b

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
			mapM_ (putStrLn . print2) $ solver' input
	where
	print2 (lid, flid, o) =
		show flid ++ " * x[0] - " ++
		show lid ++ " * x[" ++ show o ++ "] + " ++
		show o ++ " == 0"

-- Wrong
-- 1258

-- Part 1:
-- Answer = Just 410

-- The original version of solver for part 1. Harder to read.
--
-- solver :: Timestamp -> [Input] -> Maybe Integer
-- solver start input =
-- 	fmap ( uncurry (*) . -- Multiply the busId by the amount of minutes waited
-- 		(getBusId ***
-- 			-- Computes the time amount of minutes waited
-- 			subtract (getTimestamp start))) $
-- 	-- Find the earliest departure time among all the bus lines.
-- 	L.fold ( L.minimumBy (compare `on` snd) ) $
-- 	-- Find the earliest departure time after `start` for each bus line.
-- 	mapMaybe (>>= mapper) input
-- 	where
-- 	-- The Maybe arises from the find function.
-- 	mapper :: BusId -> Maybe (BusId, Integer)
-- 	mapper bid@(BusId n) = (bid,) <$> DL.find (>= getTimestamp start) (iterate (+n) 0)
