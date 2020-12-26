{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
module Day20 ( runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (anyChar, space, notChar, decimal, string, char, endOfLine)

import Data.Array.IArray (Array)
import Data.Array.IArray ((!))
import qualified Data.Array.IArray as IA
import qualified Data.Complex as DC
import qualified Linear as L
import Control.Arrow
import qualified Data.List as DL
import Control.Lens
import Data.Function (on)
import Data.Maybe (fromJust)
import Control.Monad (guard)

import Debug.Trace

type Picture = Array (L.V2 Int) Char
type Input = (Int, Picture)

solver input = input

configurations :: [Input] -> [[Input]]
configurations input = do
	-- The candidate for position 1,1
	--(p11, candidates11) <- fromJust . DL.uncons <$> DL.permutations input
	(front, p11 : rest) <- fmap (`DL.splitAt` input) [0..length input - 1]

	traceShowM (fst p11)
	traceM (printPic $ snd p11)

	-- The candidate transformation
	trans <- transforms

	--let tp11 = second trans p11

	--(p21, _) <- belowCandidate (snd tp11) candidates11

	pure undefined
	where
	(⮟) = match Below
	(⮞) = match Sideways

belowCandidate :: Picture -> [Input] -> [(Input, [Input])]
belowCandidate refPic candidates = do
	(front, cand : rest) <- fmap (`DL.splitAt` candidates) [0..length candidates - 1]
	trans <- transforms
	let tcand = second trans cand
	guard (match Below refPic $ snd cand)
	pure (cand, front ++ rest)

data RelPos = Below | Sideways deriving Show

match :: RelPos -> Picture -> Picture -> Bool
match Below refPic testPic = and $ zipWith (==)
	(fmap ( (testPic !) . (`L.V2` rows)) [1..cols]) -- the bottom row of refPic
	(fmap ( (testPic !) . (`L.V2` 1)) [1..cols]) -- the top row of testPic
	where
	(_, L.V2 cols rows) = IA.bounds refPic


-- Al valid transformations for the pictures
transforms :: [Picture -> Picture]
transforms = [id] ++ rotations ++ flips ++ zipWith (.) flips rotations
	where
	rotations = fmap (🥏) [ClockWise, CounterClockWise]
	flips = fmap (🤸) [Horizontal, Vertical]

data RotDir = ClockWise | CounterClockWise deriving (Show, Eq)

--`-rotp`
(🥏)  :: RotDir -> Picture -> Picture
(🥏)  rotDir pic = IA.array (org, L.V2 ylim xlim) $
	--traceShow (org,(ylim, xlim)) . traceShowId . first (untranslate . rotator rotDir . translate) <$> IA.assocs pic
	first (untranslate . rotator rotDir . translate) <$> IA.assocs pic
	where

	translate :: L.V2 Int -> L.V2 Float
	translate (L.V2 x y) = L.V2 (fromIntegral x - 1 - fxlim) (fromIntegral y - 1 - fylim)

	untranslate :: L.V2 Float -> L.V2 Int
	untranslate (L.V2 x y) = round <$> L.V2 (x + 1 + fxlim) (y + 1 + fylim)

	rotator :: RotDir -> L.V2 Float -> L.V2 Float
	rotator CounterClockWise (L.V2 x y) = L.V2 y (negate x)
	rotator ClockWise (L.V2 x y) = L.V2 (negate y) x

	--L.V2 (negate y) x
	(org, L.V2 xlim ylim) = IA.bounds pic
	fxlim :: Float
	fylim :: Float
	(fxlim, fylim) = (fromIntegral (xlim - 1) / 2, fromIntegral (ylim - 1) / 2)

data FlipAxis = Vertical | Horizontal deriving Show

--`-flpp`
(🤸) :: FlipAxis -> Picture -> Picture
(🤸) flipAxis pic = IA.array (org, L.V2 ylim xlim) $
	--traceShow (org,(ylim, xlim)) . traceShowId . first (untranslate . rotator rotDir . translate) <$> IA.assocs pic
	first (untranslate . flipper flipAxis . translate) <$> IA.assocs pic

	where

	translate :: L.V2 Int -> L.V2 Float
	translate (L.V2 x y) = L.V2 (fromIntegral x - 1 - fxlim) (fromIntegral y - 1 - fylim)

	untranslate :: L.V2 Float -> L.V2 Int
	untranslate (L.V2 x y) = round <$> L.V2 (x + 1 + fxlim) (y + 1 + fylim)

	flipper :: FlipAxis -> L.V2 Float -> L.V2 Float
	flipper Horizontal (L.V2 x y) = L.V2 x (negate y)
	flipper Vertical (L.V2 x y) = L.V2 (negate x) y

	--L.V2 (negate y) x
	(org, L.V2 xlim ylim) = IA.bounds pic
	fxlim :: Float
	fylim :: Float
	(fxlim, fylim) = (fromIntegral (xlim - 1) / 2, fromIntegral (ylim - 1) / 2)



-- | Parse the input into a 2D map of chars using a default character if some
-- lines are shorter
parsePicture :: Char -> Parser Picture
parsePicture def = do
	parsedLines <- parseLines
	let maxRowLength =  (maximum . fmap length) parsedLines
	let nRows = length parsedLines
	--traceShowM maxRowLength
	--traceShowM nRows
	pure $ (IA.array (L.V2 1 1, L.V2 maxRowLength nRows) . concatMap mapper . zip [1..nRows] . fmap (makeRow maxRowLength)) parsedLines
	--concatMap mapper [1 .. nRows] . fmap (makeRow maxRowLength)) parsedLines
	where
	mapper :: (Int, [(Int, Char)]) -> [(L.V2 Int, Char)]
	mapper (y, row) = fmap (first (`L.V2` y)) row
	parseLines :: Parser [[Char]]
	parseLines = parseLine `DAB.sepBy1'` endOfLine
	parseLine :: Parser [Char]
	parseLine = DAB.many1' (notChar '\n')
	makeRow :: Int -> [Char] -> [(Int, Char)]
	makeRow size content = zip [1..size] (content ++ repeat def)

parseInput :: Parser [Input]
parseInput = DAB.many1' parseSingle
	where
	parseSingle = (,) <$>
		(string "Tile " *> decimal <* char ':' <* endOfLine) <*>
		parsePicture 'X' <* DAB.choice [endOfLine <* endOfLine, endOfLine]

runSolution :: FilePath -> IO ()
runSolution filePath = do
	putStrLn "**Day 20**"
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn $ "Error :" ++ err
		Right input -> do
			--let samplePic = snd $ head input
			--putStrLn $ printPic samplePic
			--putStrLn $ printPic $ ClockWise 🥏 samplePic
			--putStrLn $ printPic $ CounterClockWise 🥏 samplePic
			--putStrLn $ printPic $ Vertical 🤸 samplePic
			--putStrLn $ printPic $ Horizontal 🤸 samplePic
			print $ head $ configurations input

printPic :: Picture -> String
printPic pic = unlines $ fmap (snd <$>) $ DL.groupBy ((==) `on` ((^. _2) . fst)) $ DL.sortOn ((^. _2) . fst) $  IA.assocs pic

