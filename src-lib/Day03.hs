{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day03  where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (notChar, endOfLine)

import qualified Data.List as DL

import qualified Data.Array.IArray as IA

import Debug.Trace

type Input = IA.Array Int (IA.Array Int Char)

circIdx :: IA.Array Int a -> Int -> a
circIdx a i = a IA.! ((i - 1) `mod` b + 1)
	where
	(_, b) = IA.bounds a

data MapObj = T | G deriving (Show, Eq)

-- | Generates the solution from the input
-- part 1
solver :: Int -> Int -> Input -> Int
solver right down input = foo
	where
	(_, size) = IA.bounds input
	thisWhole = takeWhile ((<= size) . fst) $ whole right down
	foo = DL.foldl' folder 0 thisWhole
	folder :: Int -> (Int, Int) -> Int
	folder acc (row, col) =
		acc + fromEnum ( '#' == ( input IA.! row) `circIdx` col )

-- The sequence of positions starting from (1, 1)
whole :: Int -> Int -> [(Int, Int)]
whole = go (1, 1)
	where
	go (r, c) right down = let next = (r + down, c + right) in next : go next right down

-- part 2
solver' :: Input -> Int
solver' input = product
	[ solver 1 1 input
	, solver 3 1 input
	, solver 5 1 input
	, solver 7 1 input
	, solver 1 2 input]

-- | Parse the input into a 2D map of chars using a default character if some
-- lines are shorter
parseMap :: Char -> Parser (IA.Array Int (IA.Array Int Char))
parseMap def = do
	parsedLines <- parseLines
	let maxRowLength =  (maximum . fmap length) parsedLines
	let nRows = length parsedLines
	traceShowM maxRowLength
	traceShowM nRows
	pure $ (IA.array (1, nRows) . zip [1 .. nRows] . fmap (makeRow maxRowLength)) parsedLines
	where
	parseLines :: Parser [[Char]]
	parseLines = parseLine `DAB.sepBy` endOfLine
	parseLine :: Parser [Char]
	parseLine = DAB.many' (notChar '\n')
	makeRow :: Int -> [Char] -> IA.Array Int Char
	makeRow size content = IA.array (1, size) $ zip [1..size] (content ++ repeat def)

runSolution :: FilePath -> IO ()
runSolution filePath = do
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly (parseMap 'X') contents
	case parseResult of
		Left err -> putStrLn err
		Right input -> do
			putStrLn $ unlines $ fmap IA.elems (IA.elems input)
			print $ solver' input
