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

type Input = (Int, Int, Char, String)

-- | Generates the solution from the input
solver :: [Input] -> Int
solver input = length $ filter isValid' input

parseLine :: Parser Input
parseLine = (,,,) <$>
	(read <$> DAB.many' digit) <*
	char '-' <*>
	(read <$> DAB.many' digit) <*
	char ' ' <*>
	anyChar <*
	char ':' <* char ' ' <*>
	DAB.many' letter_ascii

-- Validation for part 1
isValid :: Input -> Bool
isValid (low, high, c, str) = low <= amount && amount <= high
	where
	amount = length $ filter (==c) str

data Stage = Undecided | OkSoFar | Ok | Unacceptable deriving (Show, Eq)

-- Validation for part 2
isValid' :: Input -> Bool
isValid' (ilow, ihigh, c, str) = (== Ok) $ DL.foldl' folder Undecided $ zip [1..] str
	where
	folder :: Stage -> (Int, Char) -> Stage
	folder Undecided (idx, sc)
		| idx == low && sc == c = OkSoFar
		| idx == high && sc == c = Ok
		| otherwise = Undecided
	folder OkSoFar (idx, sc)
		| idx == high && sc /= c = Ok
		| idx == high && sc == c = Unacceptable
		| otherwise = OkSoFar
	folder Ok _ = Ok
	folder Unacceptable _ = Unacceptable

	(low, high)
		| ilow <= ihigh = (ilow, ihigh)
		| otherwise = (ihigh, ilow)


-- | One Integere per line
parseInput :: Parser [Input]
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
