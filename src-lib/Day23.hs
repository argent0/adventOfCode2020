module Day23 ( runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (digit, anyChar, space, notChar, decimal, string, char, endOfLine)
import qualified Data.List as DL


import Debug.Trace

data Digit
	= One
	| Two
	| Three
	| Four
	| Five
	| Six
	| Seven
	| Eight
	| Nine
	deriving (Show, Eq, Ord, Enum, Bounded)



type Input = String

solver input =
	DL.uncons input >>= \x -> pure $ foldr folder x [1..10]
	where
	folder _ (current, c1:c2:c3:cups) =
		let
			destination = dropWhile
		in undefined

parseInput :: Parser Input
parseInput = DAB.many1' digit <* endOfLine

runSolution :: FilePath -> IO ()
runSolution filePath = do
	putStrLn "**Day 23**"
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn $ "Error :" ++ err
		Right input -> do
			print $ solver input

	-- print $ solver $ parseQ $ lines contents
