{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
module Day20 ( runSolution) where

import Data.ByteString (ByteString)
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
import qualified Control.Foldl as L

import Debug.Trace

type Picture = Array (L.V2 Int) Char

type Piece = (Int, Picture)

-- A `FittingRow` is a list of pieces that fit side by side in an horizontal
-- line.
type FittingRow = [Piece]

-- A `FittingPuzzle` is a list of rows where each row matches the one above and
-- below.
type FittingPuzzle = [FittingRow]

data HorizontalGrowthDirection = ToTheLeft | ToTheRight deriving Show
data VerticalGrowthDirection = Upwards | Downwards deriving Show

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
-- if HorizontalGrowthDirection is ToTheLeft. P1 fits to the left of P0
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
topHorizontalSolution :: HorizontalGrowthDirection -> Piece -> [(Int, [Picture])] -> [FittingRow]
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
	bsol <- rowSolution Downwards bottomPiece ts
		(filter (not . null . snd) $ removePiece bottomPiece pieces)
	CM.guard (length (bottomPiece : bsol) == 12)
	[bottomPiece : bsol] : do
		let remPieces = DL.foldl' (flip removePiece) pieces (bottomPiece : bsol)
		bsol' <- downwardSolution (bottomPiece : bsol) remPieces
		[ (bottomPiece : bsol) : bsol']
	where

	filterBelowMatches (pid, trans) = (pid,) <$> filter (match Below (snd t)) trans 

-- | Build a row at the bottom or top of another.
--
-- Given a row of pieces (that fit together, although this funtion doesn't check
-- that this is the case)
--
-- T1 T2 T3 T4
--
-- And a piece P0 (that could match a possible T0 to the left of T1). It builds
-- all possible solutions (S1..) that fit to the right and below the T's.
--
--    T1 T2 T3 T4
-- P0 S1 S2 S3 S4
--
-- So S1 fits P0 from the right and T1 from below. Similarly T2, etc...
--
-- This function is not greedy. It returns all possible solutions. In the
-- running example that is:
--
-- [[], [S1], [S1, S2], [S1, S2, S3], [S1, S2, S3, S4]]
--
-- If there are more solutions, thouse too should be included on the list.
--
-- The solution is built to the right.
--
-- In case the VerticalGrowthDirection is Upwards the above example becomes:
--
-- P0 S1 S2 S3 S4
--    T1 T2 T3 T4
--
-- This function can be enhanced to build solutions to the left.
rowSolution :: VerticalGrowthDirection -> Piece -> FittingRow -> [(Int, [Picture])] -> [FittingRow]
rowSolution growthDir (_, sidePic) topRow pieces = unfoldrM unfolder (sidePic, (topRow, pieces))
	where
	unfolder :: (Picture, (FittingRow, [(Int, [Picture])])) ->
		[Maybe (Piece, (Picture, (FittingRow, [(Int, [Picture])])))]
	unfolder (_, ([], _)) = [Nothing]
	-- `refPic` is P0 in the comment of the parent function. It changes to (S1,
	-- S2,...) as the computation advances.
	--
	-- `(t : ts)` is the top (T1, T2, ...). It is consumed one by one.
	--
	-- `pieces` are the available pieces. The ones used are removed from the
	-- list for the next iteration.
	unfolder (refPic, (t : ts, pieces)) = (Nothing :) $
		fmap
			(\sidePiece -> Just (sidePiece,
				( snd sidePiece
				,	( ts
					, removePiece sidePiece pieces))))
			(concatMap filterBelowMatches pieces)
		where

		filterBelowMatches (pid, trans) = (pid,) <$> filter selector trans

		-- True if `p` matches P0 from the right and T1 from below/above.
		selector :: Picture -> Bool
		selector p = match Sideways refPic p && (
			case growthDir of
				Downwards -> match Below (snd t) p
				Upwards -> match Below p (snd t) )

-- | Build a solution adding a new row on the top
upwardSolution :: FittingRow -> [(Int, [Picture])] -> [FittingPuzzle]
upwardSolution _ [] = [[]]
upwardSolution (b : bs) pieces@(_ : _) = [] : do
	topPiece <- pieces >>= filterUpwardsMatches
	tsol <- rowSolution Upwards topPiece bs
		(filter (not . null . snd) $ removePiece topPiece pieces)
	CM.guard (length (topPiece : tsol) == 12)
	[topPiece : tsol] : do
		let remPieces = DL.foldl' (flip removePiece) pieces (topPiece : tsol)
		tsol' <- upwardSolution (topPiece : tsol) remPieces
		[tsol' ++ [topPiece : tsol]]
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

-- | Removes the edges from a picture.
removeEdge :: Picture -> Picture
removeEdge pic = IA.array (L.V2 1 1, L.V2 (xlim - 2) (ylim - 2)) $
		zip (fmap (+ L.V2 (-1) (-1)) nonBorderPixels) (fmap (pic !) nonBorderPixels)
	where
	nonBorderPixels = L.V2 <$> [(orgx + 1)..(xlim-1)] <*> [(orgy + 1)..(ylim-1)]
	(L.V2 orgx orgy, L.V2 xlim ylim) = IA.bounds pic

-- | Construct one big picture from a fitting puzzle
bigPicture :: FittingPuzzle -> Picture
bigPicture pzl = IA.array (L.V2 1 1, L.V2 xlim ylim) $ concatMap (\(y, cs) ->
	fmap (first (`L.V2` y)) cs)  $ zip [1..] $ fmap (zip [1..]) picStrs
	where
	ylim = length picStrs
	xlim = length $ head picStrs
	-- Fold all rows together
	picStrs = concatMap rows pictures
	-- Build a big-row
	rows :: [Picture] -> [String]
	rows pcs = DL.foldl' rowFolder [] pcs
	rowFolder :: [String] -> Picture -> [String]
	rowFolder [] p = pl p
	rowFolder acc@(_ : _) p = zipWith (++) acc (pl p)
	-- pictures without borders
	pictures :: [[Picture]]
	pictures = fmap (fmap (removeEdge . snd)) pzl
	-- picture lines
	pl :: Picture -> [String]
	pl p = fmap (fmap snd) $ DL.groupBy ((==) `on` ((^. _2) . fst)) $ DL.sortOn ((^. _2) . fst) $  IA.assocs p

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

-- | Cound the #'s present in a picture
-- Right answer is 1607
waterRoughness :: Picture -> Int
waterRoughness = length . filter ((=='#') . snd) . IA.assocs

-- | Detect the monsters and mark them with 'O'
detectMonsters :: Picture -> Picture
detectMonsters bigPic = bigPic IA.// fmap (,'O') monsters
	where
	-- The positions of the parts of the monsters
	monsters = concatMap (fmap fst) $ filter (all ((=='#') . snd))  $ fmap mapper origins
	mapper origin = fmap ( (id &&& (bigPic !)) . (+ origin)) monsterMold
	-- All valid positions of the mosnter mold in the bigPic
	origins = L.V2 <$> [picMinX..picMaxX - maxx] <*> [picMinY..picMaxY - maxy]
	(L.V2 picMinX picMinY, L.V2 picMaxX picMaxY) = IA.bounds bigPic
	-- The width and height of the moster mold
	(Just maxx, Just maxy) = L.fold ((,) <$> L.premap (^. L._x) L.maximum <*> L.premap (^. L._y) L.maximum) monsterMold
	-- The positions where the seaMonsterInput has a '#'
	-- Starts at 0, 0 becase it's intended to be uses as an offset from an
	-- origin.
	monsterMold :: [L.V2 Int]
	monsterMold = concatMap (\(y, xs) -> fmap ((`L.V2` y) . fst) xs ) $
		zip [0..] $
		fmap
			( filter ((== '#') . snd) . zip [0..] )
			seaMonsterInput

runSolution :: FilePath -> IO ()
runSolution filePath = do
	putStrLn "**Day 20**"
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn $ "Error :" ++ err
		Right input -> do
			CM.forM_ (take 1 $ solver input) $ \ pzl -> do
				putStrLn $ Boxes.render $ picLines $ bigPicture pzl
				CM.forM_ (transforms $ bigPicture pzl) $ print . waterRoughness . detectMonsters
				putStrLn "-- End Solution --"
	putStrLn "Done"

seaMonsterInput :: [String]
seaMonsterInput =
 [ "                  # "
 , "#    ##    ##    ###"
 , " #  #  #  #  #  #   " ]

puzzleLines :: FittingPuzzle -> Boxes.Box
puzzleLines pzl = Boxes.vsep 0 Boxes.left pls
	where
	pls = fmap (Boxes.hsep 1 Boxes.top) $ fmap pieceLines <$> pzl

pieceLines :: Piece -> Boxes.Box
pieceLines (pid, pic) = Boxes.text (show pid) Boxes.// picLines pic

picLines :: Picture -> Boxes.Box
picLines pic = Boxes.vcat Boxes.left $ fmap (Boxes.text . (snd <$>)) $
	DL.groupBy ((==) `on` ((^. _2) . fst)) $ DL.sortOn ((^. _2) . fst) $  IA.assocs pic

data RotDir = ClockWise | CounterClockWise deriving (Show, Eq)

--`#rotp`
(🥏)  :: RotDir -> Picture -> Picture
(🥏)  rotDir pic = IA.array (org, L.V2 ylim xlim) $
	first (untranslate . rotator rotDir . translate) <$> IA.assocs pic
	where

	translate :: L.V2 Int -> L.V2 Float
	translate (L.V2 x y) = L.V2 (fromIntegral x - 1 - fxlim) (fromIntegral y - 1 - fylim)

	untranslate :: L.V2 Float -> L.V2 Int
	untranslate (L.V2 x y) = round <$> L.V2 (x + 1 + fxlim) (y + 1 + fylim)

	rotator :: RotDir -> L.V2 Float -> L.V2 Float
	rotator CounterClockWise (L.V2 x y) = L.V2 y (negate x)
	rotator ClockWise (L.V2 x y) = L.V2 (negate y) x

	(org, L.V2 xlim ylim) = IA.bounds pic
	fxlim :: Float
	fylim :: Float
	(fxlim, fylim) = (fromIntegral (xlim - 1) / 2, fromIntegral (ylim - 1) / 2)

data FlipAxis = Vertical | Horizontal deriving Show

--`#flpp`
(🤸) :: FlipAxis -> Picture -> Picture
(🤸) flipAxis pic = IA.array (org, L.V2 ylim xlim) $
	first (untranslate . flipper flipAxis . translate) <$> IA.assocs pic

	where

	translate :: L.V2 Int -> L.V2 Float
	translate (L.V2 x y) = L.V2 (fromIntegral x - 1 - fxlim) (fromIntegral y - 1 - fylim)

	untranslate :: L.V2 Float -> L.V2 Int
	untranslate (L.V2 x y) = round <$> L.V2 (x + 1 + fxlim) (y + 1 + fylim)

	flipper :: FlipAxis -> L.V2 Float -> L.V2 Float
	flipper Horizontal (L.V2 x y) = L.V2 x (negate y)
	flipper Vertical (L.V2 x y) = L.V2 (negate x) y

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
