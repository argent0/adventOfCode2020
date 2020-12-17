module Day19 ( runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB

type Input = Integer

solver input = input

parseInput :: Parser [Input]
parseInput = undefined

parseQ :: [String] -> [Input]
parseQ = undefined

runSolution :: FilePath -> IO ()
runSolution filePath = do
	putStrLn "**Day 19**"
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn $ "Error :" ++ err
		Right input -> do
			print $ solver input

	-- print $ solver $ parseQ $ lines contents
