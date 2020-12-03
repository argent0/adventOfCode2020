{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day03  where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (digit, endOfLine, char, anyChar, letter_ascii)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as DL

import qualified Data.Array.IArray as IA

import Debug.Trace

toArray :: Input -> IA.Array Int MapObj
toArray input = IA.array (1, length input) $ zip [1..length input] input

circIdx :: IA.Array Int a -> Int -> a
circIdx a i = a IA.! ((i - 1) `mod` b + 1)
	where
	(_, b) = IA.bounds a

data MapObj = T | G deriving (Show, Eq)
type Input = [MapObj]

-- | Generates the solution from the input
-- part 1
solver :: Int -> Int -> [Input] -> Int
solver right down input = foo
	where
	arrs :: [IA.Array Int MapObj]
	arrs = toArray <$> input
	arrmap :: IA.Array Int (IA.Array Int MapObj)
	arrmap = IA.array (1, length arrs) $ zip [1..length arrs] arrs
	thisWhole = takeWhile (\(row, col) -> row <= length arrs) $ whole right down
	foo = DL.foldl' folder 0 thisWhole
	folder :: Int -> (Int, Int) -> Int
	folder acc (row, col) =
		acc + fromEnum ( T == ( (arrmap IA.! row) `circIdx` col ))

-- The sequence of positions starting from (1, 1)
whole :: Int -> Int -> [(Int, Int)]
whole = go (1, 1)
	where
	go (r, c) right down = let next = (r + down, c + right) in next : go next right down

-- part 2
solver' :: [Input] -> Int
solver' input = product
	[ solver 1 1 input
	, solver 3 1 input
	, solver 5 1 input
	, solver 7 1 input
	, solver 1 2 input]

parseLine :: Parser Input
parseLine = DAB.many' parseMapObj

parseMapObj = do
	c <- DAB.choice [char '.', char '#']
	case c of
		'.' -> pure G
		'#' -> pure T

-- | One Integere per line
parseInput :: Parser [Input]
parseInput = parseLine `DAB.sepBy` endOfLine

runSolution :: FilePath -> IO ()
runSolution filePath = do
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn err
		Right input -> do
			print [1..9]
			putStrLn $ unlines $ fmap show input
			print $ solver' $ init input
