{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Day16 ( runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (decimal, string, char, endOfLine)
import qualified Data.Attoparsec.ByteString.Char8 as DABC8

import Data.Either (rights)
import Data.Functor (($>))

type Ticket = [Int]
type Input = [Ticket]
data Rule = Rule String (Int, Int) (Int, Int) deriving Show

solver input = input

-- Put each field into its own column
columns :: Input -> [[Int]]
columns (t:ts) = foldr folder (fmap (:[]) t) ts
	where
	folder :: [Int] -> [[Int]] -> [[Int]]
	folder = zipWith (:)

parseTicket :: Parser Ticket
parseTicket = 
	( DABC8.decimal `DAB.sepBy` DABC8.char ',' ) <* DABC8.endOfLine

parseRule :: Parser Rule
parseRule = Rule <$> DAB.many1' (DABC8.notChar ':') <*>
	( DABC8.string ": " *> parseRange ) <*>
	( DABC8.string " or " *> parseRange)
	<* DABC8.endOfLine
	where
	parseRange = (,) <$> decimal <*> (char '-' *> decimal)

parseInput :: Parser ([Rule], Ticket, Input)
parseInput = do
	rules <- DAB.many1' parseRule DAB.<?> "Rules"
	_ <- DABC8.endOfLine
	_ <- DABC8.string "your ticket:" <* DABC8.endOfLine
	yt <- parseTicket
	_ <- DABC8.endOfLine
	_ <- DABC8.string "nearby tickets:" <* DABC8.endOfLine
	nbt <- DAB.many1' parseTicket
	return (rules, yt, nbt)

runSolution :: FilePath -> IO ()
runSolution filePath = do
	putStrLn "**Day 16**"
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn $ "Error :" ++ err
		Right (rules, myTicket, input) -> do
			print $ columns input
