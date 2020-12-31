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

import Data.Array.IArray (Array, (!))
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

-- A `FittingRow` is a list of pieces that fit side by side in an horizontal
-- line.
type FittingRow = [Piece]

-- A `FittingPuzzle` is a list of rows where each row matches the one above and
-- below.
type FittingPuzzle = [FittingRow]

data GrowthDirection = ToTheLeft | ToTheRight deriving Show

-- | Build a solution adding pieces to the right
--
-- For example, starting with P0 it adds P1 onwards where
-- P1 matches the *right* edge of P0.
--
-- (P0) P1 P2 P3
--
-- It returns all posibilities. That is
--
-- [] : Don't add more pieces
-- [P1] : Just add P1
-- [P1, P2] : Just add P1 and P2
-- ...
--
-- This leaves free pieces to build other parts of the solution. E.g: to the
-- left of P0.
--
-- It doesn't include the starting piece (P0)
--
-- if GrowthDirection is ToTheLeft. P1 fits to the left of P0
--
-- P1 (P0)
--
-- regardless the resulting rows are in reverse order. That is for:
--
-- P3 P2 P1 (P0)
--
-- the output is [[],[P1],[P1,P2],[P1,P2,P3]]
--
-- This means that in this case the alternatives shouls be reversed to be
-- displayed in the correct order.
topHorizontalSolution :: GrowthDirection -> Piece -> [(Int, [Picture])] -> [FittingRow]
topHorizontalSolution growthDir (_, topLeftPic) pieces = unfoldrM unfolder (topLeftPic, pieces)
	where
	unfolder :: (Picture, [(Int, [Picture])]) -> [Maybe (Piece, (Picture, [(Int, [Picture])]))]
	-- `refPic` is the picture the next piece should fit on the side.
	-- `pieces` are the remmaining unused pieces
	--
	-- The alternatives are:
	-- 	Stop
	-- 	One alternative for each piece that matches the refPic on the side.
	unfolder (refPic, pieces) = (Nothing :) $
		fmap
			(\sidePiece -> Just (sidePiece,
				( snd sidePiece -- The next refPic
				, removePiece sidePiece pieces))) -- The next pieces
			(concatMap filterSideMatchs pieces)
		where

		filterSideMatchs :: (Int, [Picture]) -> [(Int, Picture)]
		filterSideMatchs (pid, trans) = (pid,) <$> filter selector trans

		selector = case growthDir of
			ToTheRight -> match Sideways refPic
			ToTheLeft -> (\t -> match Sideways t refPic)

-- | Build a solution adding pieces to both sides
topMiddleSolution :: (Int, Picture) -> [(Int, [Picture])] -> [FittingRow]
topMiddleSolution topCenter@(_, topCenterPic) pieces = [topCenter] : do
	rightPiece <- pieces >>= filterRightMatches
	leftPiece <- removePiece rightPiece pieces >>= filterLeftMatches

	let remPieces = removePiece rightPiece (removePiece leftPiece pieces)

	rightSolution <- topHorizontalSolution ToTheRight rightPiece remPieces

	-- The soultions need to be reversed because they are to the left of the
	-- `topCenter` piece.
	leftSolution <- fmap reverse $ topHorizontalSolution ToTheLeft leftPiece $
		DL.foldl' (flip removePiece) remPieces rightSolution

	pure (leftSolution ++ [leftPiece, topCenter, rightPiece] ++ rightSolution)
	where
	filterRightMatches :: (Int, [Picture]) -> [(Int, Picture)]
	filterRightMatches (pid, trans) = (pid,) <$> filter (match Sideways topCenterPic) trans

	filterLeftMatches :: (Int, [Picture]) -> [(Int, Picture)]
	filterLeftMatches (pid, trans) = (pid,) <$> filter (\t -> match Sideways t topCenterPic) trans

-- | Build a solution adding a new row at the bottom
downwardSolution :: FittingRow -> [(Int, [Picture])] -> [FittingPuzzle]
downwardSolution _ [] = [[]]
downwardSolution (t : ts) pieces@(_ : _) = [] : do
	bottomPiece <- pieces >>= filterBelowMatches
	bsol <- bottomRowSolution ts bottomPiece
		(filter (not . null . snd) $ removePiece bottomPiece pieces)
	CM.guard (length (bottomPiece : bsol) == 12)
	[bottomPiece : bsol] : do
		let remPieces = DL.foldl' (flip removePiece) pieces (bottomPiece : bsol)
		bsol' <- downwardSolution (bottomPiece : bsol) remPieces
		[ (bottomPiece : bsol) : bsol']
	where

	filterBelowMatches (pid, trans) = (pid,) <$> filter (match Below (snd t)) trans 

-- | Build a row at the bottom of another.
bottomRowSolution :: FittingRow -> Piece -> [(Int, [Picture])] -> [FittingRow]
bottomRowSolution [] _ _ = [[]]
bottomRowSolution _ _ [] = [[]]
bottomRowSolution (t : ts) bottomPiece pieces@(_ : _) = [] : do
	candidate <- pieces >>= filterBelowMatches
	CM.guard (match Sideways (snd bottomPiece) (snd candidate))
	candidateSolution <- bottomRowSolution ts candidate (removePiece candidate pieces)
	pure $ candidate : candidateSolution
	where

	filterBelowMatches (pid, trans) = (pid,) <$> filter (match Below (snd t)) trans

-- | Build a solution adding a new row on the top
upwardSolution :: FittingRow -> [(Int, [Picture])] -> [FittingPuzzle]
upwardSolution _ [] = [[]]
upwardSolution (b : bs) pieces@(_ : _) = [] : do
	topPiece <- pieces >>= filterUpwardsMatches
	tsol <- topRowSolution bs topPiece
		(filter (not . null . snd) $ removePiece topPiece pieces)
	CM.guard (length (topPiece : tsol) == 12)
	[topPiece : tsol] : do
		let remPieces = DL.foldl' (flip removePiece) pieces (topPiece : tsol)
		tsol' <- upwardSolution (topPiece : tsol) remPieces
		[tsol' ++ [topPiece : tsol]]
	where

	filterUpwardsMatches (pid, trans) = (pid,) <$> filter (\t -> match Below t (snd b)) trans

-- | Build a row on the top of another.
topRowSolution :: FittingRow -> Piece -> [(Int, [Picture])] -> [FittingRow]
topRowSolution [] _ _ = [[]]
topRowSolution _ _ [] = [[]]
topRowSolution topRow@(b : bs) topPiece pieces@(_ : _) = [] : do
	candidate <- pieces >>= filterUpwardsMatches
	CM.guard (match Sideways (snd topPiece) (snd candidate))
	[candidate] : do
		candidateSolution <- topRowSolution bs candidate (removePiece candidate pieces)
		pure $ candidate : candidateSolution
	where

	filterUpwardsMatches (pid, trans) = (pid,) <$> filter (\t -> match Below t (snd b)) trans

middleRowSolution :: FittingRow -> [(Int, [Picture])] -> [FittingPuzzle]
middleRowSolution middleRow pieces = [middleRow] : do
	ts <- upwardSolution middleRow pieces
	let remPieces = DL.foldl' (flip removePiece) pieces (concat ts)
	bs <- downwardSolution middleRow remPieces
	pure $ ts ++ [middleRow] ++ bs

removePiece :: Piece -> [(Int, [Picture])] -> [(Int, [Picture])]
removePiece piece = filter ((/= fst piece) . fst)

solver input = case transformed of
	[] -> []
	(p : ps) -> do
		trans <- snd p
		topRow <- topMiddleSolution (fst p, trans) ps
		CM.guard (length topRow == 12)
		let remPieces = DL.foldl' (flip removePiece) (p : ps) topRow
		bs <- middleRowSolution topRow remPieces
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
	-- try to exploit the picture's symmetry. Flip symmetries.
	Set.toList $ Set.fromList $ 
	-- An Horizontal flip is a Vertical flip and a 180 degree rotation.
	rotations ++ ( (Vertical 🤸) <$> rotations )
	where
	-- try to exploit the picture's symmetry. Stop ASAP. 90 degree rotation or
	-- 180 degree rotation symmetry
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
			CM.forM_ (take 1 $ solver input) $ \ pzl -> do
				putStrLn $ Boxes.render $ puzzleLines pzl
				putStrLn "-- End Solution --"
	putStrLn "Done"

puzzleLines :: FittingPuzzle -> Boxes.Box
puzzleLines pzl = Boxes.vsep 0 Boxes.left pls
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

-- | unfoldr in a monad.
--
-- This is missing from Data.List
unfoldrM :: Monad m => (a -> m (Maybe (b, a))) -> a -> m [b]
unfoldrM f a = f a >>= \case
	Nothing -> pure []
	Just (b, a') -> (b :) <$> unfoldrM f a'
