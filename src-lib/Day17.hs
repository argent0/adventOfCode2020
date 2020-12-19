{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Day17 ( runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (notChar, decimal, string, char, endOfLine)
import qualified Data.Attoparsec.ByteString.Char8 as DABC8

import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.List as DL

import qualified Linear as L

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Arrow
import Data.Maybe (mapMaybe, catMaybes)
import Debug.Trace

-- Only keep track of the active cells

type Input = Vector (Vector Char)

solver input samples =
	take samples $ iterate iterator (auto, length $ filter (=='#') $ Map.elems auto)
	where
	auto = seed input
	iterator (a, _) = (evolve a, length $ filter (=='#') $ Map.elems a)

-- part 2
solver' input samples = 
	take samples $ iterate iterator (auto, length $ filter (=='#') $ Map.elems auto)
	where
	auto = seed' input
	iterator (a, _) = (evolve' a, length $ filter (=='#') $ Map.elems a)

evolve :: Automata -> Automata
evolve auto = DL.foldl' folder (Map.filter (=='#') evolvedActives) evoledInactives
	where
	folder :: Map (L.V3 Int) Char -> L.V3 Int -> Map (L.V3 Int) Char
	folder m k =  Map.insert k '#' m
	-- activate if there are 3 nieghs active
	evoledInactives =  fmap head $ filter ((==3) . length) $ DL.group $ DL.sort $ concat $ Map.elems activeNeighs
	evolvedActives = Map.map actEvolverMapper activeNeighs
	actEvolverMapper :: [L.V3 Int] -> Char
	actEvolverMapper nghs
		-- TODO: use elem [2,3]
		| 2 == length (mapMaybe (`Map.lookup` auto) nghs) = '#'
		| 3 == length (mapMaybe (`Map.lookup` auto) nghs) = '#'
		| otherwise = '.'
	-- The neighs of the actives in a map Pos -> [Pos]
	activeNeighs = Map.mapWithKey (\pos _ -> neigh pos) actives
	(actives, inactives) = Map.partition (=='#') auto

-- part 2
evolve' :: Automata' -> Automata'
evolve' auto = DL.foldl' folder (Map.filter (=='#') evolvedActives) evoledInactives
	where
	folder :: Map (L.V4 Int) Char -> L.V4 Int -> Map (L.V4 Int) Char
	folder m k =  Map.insert k '#' m
	-- activate if there are 3 nieghs active
	evoledInactives =  fmap head $ filter ((==3) . length) $ DL.group $ DL.sort $ concat $ Map.elems activeNeighs
	evolvedActives = Map.map actEvolverMapper activeNeighs
	actEvolverMapper :: [L.V4 Int] -> Char
	actEvolverMapper nghs
		| 2 == length (mapMaybe (`Map.lookup` auto) nghs) = '#'
		| 3 == length (mapMaybe (`Map.lookup` auto) nghs) = '#'
		| otherwise = '.'
	-- The neighs of the actives in a map Pos -> [Pos]
	activeNeighs = Map.mapWithKey (\pos _ -> neigh' pos) actives
	(actives, inactives) = Map.partition (=='#') auto
 
neigh :: L.V3 Int -> [L.V3 Int]
-- TODO: use (+ dx)
neigh (L.V3 x y z) = fmap (\(L.V3 dx dy dz) -> L.V3 (x+dx) (y+dy) (z+dz)) $ filter (/=L.V3 0 0 0) $ L.V3 <$> [-1,0,1] <*> [-1,0,1] <*> [-1,0,1]

-- part 2
neigh' :: L.V4 Int -> [L.V4 Int]
neigh' (L.V4 x y z w) = fmap (\(L.V4 dx dy dz dw) -> L.V4 (x+dx) (y+dy) (z+dz) (w+dw)) $ filter (/=L.V4 0 0 0 0) $ L.V4 <$> [-1,0,1] <*> [-1,0,1] <*> [-1,0,1] <*> [-1,0,1]

type Automata = Map (L.V3 Int) Char
type Automata' = Map (L.V4 Int) Char

seed :: Input -> Automata
seed input = Map.filter (=='#') $ Map.mapKeys (\v -> v - L.V3 1 1 0) $ Map.fromList coords
	where
	coords :: [(L.V3 Int, Char)]
	coords = concatMap (\(y, xcs) -> fmap (\(x, c) -> (L.V3 x y 0, c))  xcs) cells
	cells :: [(Int, [(Int, Char)])]
	cells = fmap (second (zip [0..] . Vec.toList)) rows
	rows = zip [0..] $ Vec.toList input

-- part 2
seed' :: Input -> Automata'
seed' input = Map.filter (=='#') $ Map.mapKeys (\v -> v - L.V4 1 1 0 0) $ Map.fromList coords
	where
	coords :: [(L.V4 Int, Char)]
	coords = concatMap (\(y, xcs) -> fmap (\(x, c) -> (L.V4 x y 0 0, c))  xcs) cells
	cells :: [(Int, [(Int, Char)])]
	cells = fmap (second (zip [0..] . Vec.toList)) rows
	rows = zip [0..] $ Vec.toList input

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

parseQ :: [String] -> [Input]
parseQ = undefined

showMap :: Input -> String
showMap = unlines . fmap (DL.foldr (:) [] . Vec.toList) . Vec.toList

runSolution :: FilePath -> IO ()
runSolution filePath = do
	putStrLn "**Day 17**"
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly (parseMap 'X') contents
	case parseResult of
		Left err -> putStrLn $ "Error :" ++ err
		Right ((cols, rows), input) -> do
			print (cols, rows)
			putStrLn $ showMap input
			print $ (!! 7) $ fmap snd $ solver input 10
			print $ (!! 7) $ fmap snd $ solver' input 10

	-- print $ solver $ parseQ $ lines contents
