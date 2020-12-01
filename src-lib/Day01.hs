{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day01 (runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (digit, endOfLine)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as DL

-- | Generates the solution from the input
solver :: [Int] -> Int
solver input = (\(a,(b,c)) -> a*b*c) $
	head $
	filter (\(a,(b,c)) -> c + a + b == 2020) $
	(,) <$> input <*> ( (,) <$> input <*> input)

parseInput :: Parser [Int]
parseInput =
	fmap (read :: String -> Int) . filter (not . null) <$>
		DAB.many' digit `DAB.sepBy` endOfLine

runSolution :: FilePath -> IO ()
runSolution filePath = do
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn err
		Right input ->
			print $ solver input
