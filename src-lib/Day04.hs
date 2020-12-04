{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Day04  where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (inClass, digit, endOfLine, char, anyChar, letter_ascii)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.List as DL
import qualified Data.List.Split as DLS

import qualified Data.Map.Strict as Map

import Debug.Trace

import Data.Maybe (isJust)

type Input = [(String, String)]


-- TODO: probably write a perser for part 2. Idem for part 1.

-- | Generates the solution from the input
-- part 1
solver :: [Input] -> Int
solver = length . filter valid'

valid :: Input -> Bool
valid input = DL.sort keys == mandatory || DL.sort keys == mandatoryNoCid
	where
	keys = fmap fst input
	mandatory = DL.sort ["ecl","pid","eyr","hcl","byr","iyr","cid","hgt"]
	mandatoryNoCid = DL.sort ["ecl","pid","eyr","hcl","byr","iyr","hgt"]

inInterval :: Int -> Int -> Int -> Maybe ()
inInterval min max n = if (min<= n) && (n <= max)
					then pure ()
					else Nothing

valid' :: Input -> Bool
valid' input = isJust $ do
	Map.lookup "byr" kmap >>= \byr ->
		if DL.all (inClass "0987654321") byr
			then let
				year = (read byr :: Int)
				in if (1920<= year) && (year <= 2002)
					then pure ()
					else traceShow input Nothing
			else traceShow input Nothing

	Map.lookup "iyr" kmap >>= \iyr ->
		if DL.all (inClass "0987654321") iyr
			then let
				year = (read iyr :: Int)
				in if (2010<= year) && (year <= 2020)
					then pure ()
					else traceShow input Nothing
			else traceShow input Nothing

	Map.lookup "eyr" kmap >>= \eyr ->
		if DL.all (inClass "0987654321") eyr
			then let
				year = (read eyr :: Int)
				in if (2020<= year) && (year <= 2030)
					then pure ()
					else traceShow input Nothing
			else traceShow input Nothing

	Map.lookup "hgt" kmap >>= \hgt -> do
		case  DLS.splitOneOf "ic" hgt of
			[num, units] -> if DL.all (inClass "0987654321") num
				then let
					hgtNum = (read num :: Int)
					in case units of
						"m" -> inInterval 150 193 hgtNum
						"n" -> inInterval 59 76 hgtNum
						_ -> traceShow input Nothing
				else traceShow input Nothing
			_ -> traceShow input Nothing

	Map.lookup "hcl" kmap >>= \case
		-- didn't check for lenght = 7
		('#': color) -> if length color == 6 && DL.all (inClass "0987654321abcdef") color
			then pure ()
			else traceShow input Nothing
		_ -> traceShow input Nothing
		
	Map.lookup "ecl" kmap >>= \case
		"amb" -> pure ()
		"blu" -> pure ()
		"brn" -> pure ()
		"gry" -> pure ()
		"grn" -> pure ()
		"hzl" -> pure ()
		"oth" -> pure ()
		_ -> traceShow input Nothing

	Map.lookup "pid" kmap >>= \pid ->
		if length pid == 9 && DL.all (inClass "0987654321") pid
			then pure ()
			else traceShow input Nothing
	
	where
	kmap = Map.fromList input
	mandatory = DL.sort ["ecl","pid","eyr","hcl","byr","iyr","cid","hgt"]
	mandatoryNoCid = DL.sort ["ecl","pid","eyr","hcl","byr","iyr","hgt"]

parsePass :: [String] -> Input
parsePass this = concat pairs
	where
	pairs = fmap (fmap (pairer . DLS.splitWhen (==':')) . words) this
	pairer :: [String] -> (String, String)
	pairer [k, v] = (k, v)

-- | One Integere per line
parseInput :: [String] -> [Input]
parseInput = fmap parsePass . DLS.splitWhen DL.null 

runSolution :: FilePath -> IO ()
runSolution filePath = do
	contents <- readFile filePath
	print $ solver $ parseInput $ lines contents
