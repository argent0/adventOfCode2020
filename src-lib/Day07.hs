{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day07 where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (inClass, digit, endOfLine, char, anyChar, letter_ascii)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Data.List as DL
import qualified Data.List.Split as DLS

import qualified Data.Map.Strict as Map

import Debug.Trace

import Data.Maybe (isJust)
import Control.Arrow

newtype Color = Color String deriving (Show, Eq, Ord)

type Input = (Color, [(Int, Color)])

-- | Generates the solution from the input
-- part 2
solver :: [Input] -> Integer
solver input = count (Color "shiny gold")
	where
	count :: Color -> Integer
	count color =
		case Map.lookup color directMap of
			Just [] -> 0
			Just desc ->
				traceShowId $ DL.foldl' (+) 0 $
					(fmap (fromIntegral . fst) desc) ++
					(fmap (uncurry (*) . (fromIntegral *** count)) desc)
			Nothing -> error $ show color

	directMap :: Map Color [(Int, Color)]
	directMap = Map.fromList input
	inverseMap =  Map.fromListWith (++) $ fmap (second (:[])) $ concatMap invertElems input
	invertElems :: Input -> [(Color, Color)]
	invertElems (c, cts) = fmap ((,c) . snd) cts

-- Part 1
solver' input =
	DL.foldl' (+) 0 $
		fmap (fromEnum . expand) $ filter (/= (Color "shiny gold")) $ Map.keys directMap
	where
	expand :: Color -> Bool
	expand (Color "shiny gold") = True
	expand color =
		case Map.lookup color directMap of
			Just [] -> False
			Just desc -> DL.any expand (snd <$> desc)
			Nothing -> error $ show color
	directMap :: Map Color [(Int, Color)]
	directMap = Map.fromList input
	inverseMap =  Map.fromListWith (++) $ fmap (second (:[])) $ concatMap invertElems input
	invertElems :: Input -> [(Color, Color)]
	invertElems (c, cts) = fmap ((,c) . snd) cts

-- | One Rule per line
parseQ :: [String] -> [Input]
parseQ = fmap (doer . DLS.splitWhen (=="contain") . words)
	where
	doer :: [[String]] -> Input
	doer (s:[ss]) = (Color (unwords $ DL.init s),
		concatMap (make . words . strip) $ DLS.splitWhen (==',') $ unwords ss)
	make :: [String] -> [(Int, Color)]
	make ("no":["other","bags."]) = []
	make (n:cc) = [ (read n, Color $ unwords $ DL.init cc) ]
	strip :: String -> String
	strip [] = []
	strip (' ':ss) = ss
	strip ss = ss

-- Apparently uou had to find the gap op the seatids
runSolution :: FilePath -> IO ()
runSolution filePath = do
	contents <- readFile filePath
	print $ solver $ parseQ $ lines contents
