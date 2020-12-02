{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day02 (runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (digit, endOfLine, char)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as DL

-- | Generates the solution from the input
solver :: [Integer] -> Integer
solver = DL.foldl' folder 0
	where folder = undefined


-- | One Integere per line
parseInput :: Parser [Integer]
parseInput =
	fmap (read :: String -> Integer) . filter (not . null) <$>
		DAB.many' digit `DAB.sepBy` endOfLine

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
