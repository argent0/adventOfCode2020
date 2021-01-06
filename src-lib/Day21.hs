{-# LANGUAGE OverloadedStrings #-}
module Day21 ( runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import qualified Data.Attoparsec.ByteString.Char8 as DABC8

import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow

type Input = ([Text], [Text])

solver input = input

parseInput :: Parser [Input]
parseInput = DAB.many1' (
	fmap (fmap T.pack *** fmap T.pack) . (,) <$>
	DAB.sepBy1'
		(DAB.many1' DABC8.letter_ascii)
		DABC8.space <*>
	( (DABC8.string " (contains " DAB.<?> "missing contains") *>
	DAB.sepBy'
		(DAB.many1' DABC8.letter_ascii)
		(DABC8.string ", " DAB.<?> "wrong separator?")
	<* DABC8.char ')' <* DABC8.endOfLine ))

runSolution :: FilePath -> IO ()
runSolution filePath = do
	putStrLn "**Day 21**"
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn err
		Right input ->
			print $ solver input


	-- print $ solver $ parseQ $ lines contents
