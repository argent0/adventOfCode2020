{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day02 (runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (digit, endOfLine, char, anyChar, letter_ascii)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as DL

-- | Generates the solution from the input
solver :: [(Int, Int, Char, String)] -> Int
solver input = length $ filter isValid' input


parseLine :: Parser (Int, Int, Char, String)
parseLine = do
	low <- DAB.many' digit
	_ <- char '-'
	high <- DAB.many' digit
	_ <- char ' '
	c <- anyChar
	_ <- char ':'
	_ <- char ' '
	str <- DAB.many' letter_ascii
	return (read low, read high, c, str)

isValid :: (Int, Int, Char, String) -> Bool
isValid (low, high, c, str) = low <= amount && amount <= high
	where
	amount = length $ filter (==c) str

isValid' :: (Int, Int, Char, String) -> Bool
isValid' (low, high, c, str) = (str !! (low - 1) == c) /= (str !! (high - 1) == c)


-- | One Integere per line
parseInput :: Parser [(Int, Int, Char, String)]
parseInput = parseLine `DAB.sepBy` endOfLine

-- | 1,2,3 per line
parseInput' :: Parser [[Integer]]
parseInput' =
	fmap (fmap (read :: String -> Integer)) . filter (not . null) <$>
		(DAB.many' digit `DAB.sepBy` char ',') `DAB.sepBy` endOfLine

runSolution :: FilePath -> IO ()
runSolution filePath = do
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn err
		Right input ->
			print $ solver input
