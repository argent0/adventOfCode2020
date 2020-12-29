
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
import qualified Control.Monad as CM
import qualified Data.Set as Set
import qualified Text.PrettyPrint.Boxes as Boxes

import Debug.Trace

type Picture = Array (L.V2 Int) Char
type Piece = (Int, Picture)
-- One list of pieces per line
type FittingRow = [Piece]
type FittingPuzzle = [FittingRow]

topLeftSolution :: (Int, Picture) -> [(Int, [Picture])] -> [FittingRow]
topLeftSolution topLeft [] = [[topLeft]]
topLeftSolution topLeft@(_, topLeftPic) pieces@(_ : _) = [topLeft] : do
	sidePiece <- pieces >>= filterSideMatchs
	sideSolution <- topLeftSolution sidePiece (removePiece sidePiece pieces)
	pure $ topLeft : sideSolution
	where
	filterSideMatchs :: (Int, [Picture]) -> [(Int, Picture)]
	filterSideMatchs (pid, trans) = (pid,) <$> filter (match Sideways topLeftPic) trans

topRightSolution :: (Int, Picture) -> [(Int, [Picture])] -> [FittingRow]
topRightSolution topRight [] = [[topRight]]
topRightSolution topRight@(_, topRightPic) pieces@(_ : _) = [topRight] : do
	sidePiece <- pieces >>= filterSideMatchs
	sideSolution <- topRightSolution sidePiece (removePiece sidePiece pieces)
	pure $ topRight : sideSolution
	where
	filterSideMatchs :: (Int, [Picture]) -> [(Int, Picture)]
	filterSideMatchs (pid, trans) = (pid,) <$> filter (\t -> match Sideways t topRightPic) trans

topMiddleSolution :: (Int, Picture) -> [(Int, [Picture])] -> [FittingRow]
topMiddleSolution topCenter@(_, topCenterPic) pieces = [topCenter] : do
	leftPiece <- pieces >>= filterLeftMatches
	rightPiece <- removePiece leftPiece pieces >>= filterRightMatches

	let remPieces = removePiece rightPiece (removePiece leftPiece pieces)

	leftSolution <- topLeftSolution leftPiece remPieces

	rightSolution <- topRightSolution rightPiece $
		DL.foldl' (flip removePiece) remPieces leftSolution

	pure (rightSolution ++ [topCenter] ++ leftSolution)
	where
	filterLeftMatches :: (Int, [Picture]) -> [(Int, Picture)]
	filterLeftMatches (pid, trans) = (pid,) <$> filter (match Sideways topCenterPic) trans

	filterRightMatches :: (Int, [Picture]) -> [(Int, Picture)]
	filterRightMatches (pid, trans) = (pid,) <$> filter (\t -> match Sideways t topCenterPic) trans

bottomSolution :: FittingRow -> [(Int, [Picture])] -> [FittingPuzzle]
bottomSolution topRow [] = [[topRow]]
bottomSolution topRow@(t : ts) pieces@(_ : _) = [topRow] : do
	bottomPiece <- pieces >>= filterBelowMatches
	bsol <- bottomRowSolution ts bottomPiece
		(filter (not . null . snd) $ removePiece bottomPiece pieces)
	CM.guard (length bsol == 12)
	[topRow, bsol] : do
		let remPieces = DL.foldl' (flip removePiece) pieces (bottomPiece : bsol)
		bsol' <- bottomSolution bsol remPieces
		[ topRow : bsol' ]
	where

	filterBelowMatches (pid, trans) = (pid,) <$> filter (match Below (snd t)) trans 

	filterSideMatches :: Picture -> (Int, [Picture]) -> (Int, [Picture])
	filterSideMatches refPic (pid, trans) = (pid, filter (match Sideways refPic) trans)

bottomRowSolution :: FittingRow -> Piece -> [(Int, [Picture])] -> [FittingRow]
bottomRowSolution [] bottomPiece _ = [[bottomPiece]]
bottomRowSolution topRow bottomPiece [] = [[bottomPiece]]
bottomRowSolution topRow@(t : ts) bottomPiece pieces@(_ : _) = [bottomPiece] : do
	candidate <- pieces >>= filterBelowMatches
	CM.guard (match Sideways (snd bottomPiece) (snd candidate))
	candidateSolution <- bottomRowSolution ts candidate (removePiece candidate pieces)
	pure $ bottomPiece : candidateSolution
	where

	filterBelowMatches (pid, trans) = (pid,) <$> filter (match Below (snd t)) trans 

	filterSideMatches :: Picture -> (Int, [Picture]) -> (Int, [Picture])
	filterSideMatches refPic (pid, trans) = (pid, filter (match Sideways refPic) trans)

removePiece :: Piece -> [(Int, [Picture])] -> [(Int, [Picture])]
removePiece piece = filter ((/= fst piece) . fst)

solver input = case transformed of
	[] -> []
	(p : ps) -> do
		-- p is on the top left corner
		--topRow <- topLeftSolution (second head p) ps ++ topMiddleSolution (second head p) ps
		trans <- snd p
		topRow <- topMiddleSolution (fst p, trans) ps
		CM.guard (length topRow == 12)
		let remPieces = DL.foldl' (flip removePiece) (p : ps) topRow
		bs <- bottomSolution topRow remPieces
		CM.guard (length bs == 12)
		pure bs

	where
	transformed = fmap (second transforms) input

data RelPos = Below | Sideways deriving Show

match :: RelPos -> Picture -> Picture -> Bool
match relPos refPic testPic = and $ case relPos of
	Below -> zipWith (==)
		(fmap ( (refPic !) . (`L.V2` rows)) [1..cols]) -- the bottom row of refPic
		(fmap ( (testPic !) . (`L.V2` 1)) [1..cols]) -- the top row of testPic
	Sideways -> zipWith (==)
		(fmap ( (refPic !) . (cols `L.V2`)) [1..rows]) -- the rightmost col of refPic
		(fmap ( (testPic !) . (1 `L.V2`)) [1..rows]) -- the leftmost col of testPic
	where
	(_, L.V2 cols rows) = IA.bounds refPic

-- All unique transformations of the picture
transforms :: Picture -> [Picture]
transforms basePic =
	Set.toList $ Set.fromList $ rotations ++ ( (🤸) <$> [Vertical, Horizontal] <*> rotations )
	where
	rotations = basePic : takeWhile (/= basePic) (tail $ iterate (ClockWise 🥏) basePic )

-- | Parse the input into a 2D map of chars using a default character if some
-- lines are shorter
parsePicture :: Char -> Parser Picture
parsePicture def = do
	parsedLines <- parseLines
	let maxRowLength =  (maximum . fmap length) parsedLines
	let nRows = length parsedLines
	pure $ (IA.array (L.V2 1 1, L.V2 maxRowLength nRows) . concatMap mapper . zip [1..nRows] . fmap (makeRow maxRowLength)) parsedLines
	where
	mapper :: (Int, [(Int, Char)]) -> [(L.V2 Int, Char)]
	mapper (y, row) = fmap (first (`L.V2` y)) row
	parseLines :: Parser [[Char]]
	parseLines = parseLine `DAB.sepBy1'` endOfLine
	parseLine :: Parser [Char]
	parseLine = DAB.many1' (notChar '\n')
	makeRow :: Int -> [Char] -> [(Int, Char)]
	makeRow size content = zip [1..size] (content ++ repeat def)

parseInput :: Parser [Piece]
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
			let pinput = (DL.permutations input)
			CM.forM_ (fmap solver pinput) $ \ pzl -> do
				CM.forM_ pzl $ \ pzl' -> do
					putStrLn $ Boxes.render $ puzzleLines pzl'
					putStrLn "-- End Solution --"
	putStrLn "Done"

puzzleLines :: FittingPuzzle -> Boxes.Box
puzzleLines pzl = Boxes.vsep 1 Boxes.left pls
	where
	pls = fmap (Boxes.hsep 1 Boxes.top) $ fmap pieceLines <$> pzl

pieceLines :: Piece -> Boxes.Box
pieceLines (pid, pic) = Boxes.text (show pid) Boxes.// picLines pic

picLines :: Picture -> Boxes.Box
picLines pic = Boxes.vcat Boxes.left $ fmap (Boxes.text . (snd <$>)) $ DL.groupBy ((==) `on` ((^. _2) . fst)) $ DL.sortOn ((^. _2) . fst) $  IA.assocs pic

data RotDir = ClockWise | CounterClockWise deriving (Show, Eq)

--`#rotp`
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

--`#flpp`
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
