{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day11 where

import Control.Lens
import Control.Lens.TH

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import qualified Data.Attoparsec.ByteString.Char8 as DABC8
import Data.Attoparsec.ByteString.Char8 (inClass, digit, endOfLine, char, anyChar, letter_ascii, notChar)

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

-- import qualified Control.Foldl as L

import Data.Bifunctor (bimap)

--type Input = (IA.Array Int (IA.Array Int Char))
type Input = Vector (Vector Char)

-- Solve part 2
solver rows cols input = fmap (count . fst) $ DL.find (uncurry (==)) $ zip frames $ tail frames
	where
	frames = iterate (step rows cols) input
	count :: Input -> Integer
	count = DL.foldl' (+) (0 :: Integer) . fmap (DL.genericLength . filter (=='#'). Vec.toList) . Vec.toList

-- | foo
--
-- >>> adjacents 1 1 (0,0)
-- []
-- >>> adjacents 3 3 (1,1)
-- [(2,2),(2,1),(2,0),(1,2),(1,0),(0,2),(0,1),(0,0)]
-- >>> adjacents 3 3 (2,2)
-- [(2,1),(1,2),(1,1)]
-- >>> adjacents 10 10 (3,0)
-- [(4,1),(4,0),(3,1),(2,1),(2,0)]
adjacents :: Int -> Int -> (Int, Int) -> [(Int, Int)]
adjacents rows cols p@(col, row)
	| isOOB p = error $ "col or row OOB" ++ show (cols, rows, p)
	| otherwise = filter (not . isOOB) $ fmap (\(a, b) -> (col-a, row-b)) $ filter (/=(0,0)) $ (,) <$> [-1..1] <*> [-1..1]
	where
	isOOB (col, row) =  col < 0 || row < 0 || col >= cols || row >= rows

step :: Int -> Int -> Input -> Input
step rows cols input =
	Vec.fromList $ fmap mapper $ zip [0..] $ Vec.toList input
	where
	mapper :: (Int, Vector Char) -> Vector Char
	mapper (row, rowData) = Vec.fromList $ fmap (caser row ) $ zip [0..] $ Vec.toList rowData
	caser :: Int -> (Int, Char) -> Char
	caser _ (_, '.') = '.'
	caser row (col, 'L')
		| counts(col, row) == 0 = '#'
		| otherwise = 'L'
	caser row (col, '#')
		| counts(col, row) >= 4 = 'L'
		| otherwise = '#'

	-- count the num of ocp seats around
	counts :: (Int, Int) -> Int
	counts = length . filter (== '#') . fmap (\(col, row) -> (input ! row) ! col) . adjacents rows cols

showMap :: Input -> String
showMap = unlines . fmap (DL.foldr (:) [] . Vec.toList) . Vec.toList

-- | Parse the input into a 2D map of chars using a default character if some
-- lines are shorter
parseMap :: Char -> Parser ((Int, Int), Vector (Vector Char))
parseMap def = do
	parsedLines <- parseLines
	let maxRowLength =  (maximum . fmap length) parsedLines
	let nRows = length (filter (not . null) parsedLines)
	pure $ ((maxRowLength, nRows),) $ (Vec.fromList . fmap snd . zip [1 .. nRows] . fmap (makeRow maxRowLength)) parsedLines
	where
	parseLines :: Parser [[Char]]
	parseLines = parseLine `DAB.sepBy` endOfLine
	parseLine :: Parser [Char]
	parseLine = DAB.many' (notChar '\n')
	makeRow :: Int -> [Char] -> Vector Char
	makeRow size content = Vec.fromList $ snd <$> zip [1..size] (content ++ repeat def)

runSolution :: FilePath -> IO ()
runSolution filePath = do
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly (parseMap 'X') contents
	case parseResult of
		Left err -> putStrLn err
		Right ((cols, rows), input) -> do
			print (cols, rows)
			putStrLn $ showMap input
			print $ solver rows cols input
