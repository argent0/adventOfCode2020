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

{-
 - Could use a binnary tree to make this n log n complexity.
 -
 - for every n in the input insert it in BT and search that tree for 2020-n
 -}

data Tree a
	= Empty
	| Branch !(Tree a) !a !(Tree a)

btInsert :: Ord a => a -> Tree a -> Tree a
btInsert a Empty = Branch Empty a Empty
btInsert a (Branch l o r)
	| a <= o = Branch (btInsert a l) o r
	| otherwise = Branch l o (btInsert a r)

btElem :: Ord a => a -> Tree a -> Bool
btElem _ Empty = False
btElem a (Branch l o r)
	| a == o = True
	| a < o = btElem a l
	| otherwise = btElem a r

-- | Generates the solution from the input
solver :: [Int] -> Int
solver input = (\(a,(b,c)) -> a*b*c) $
	head $
	filter (\(a,(b,c)) -> c + a + b == 2020) $
	(,) <$> input <*> ( (,) <$> input <*> input)

solver1 :: [Int] -> Maybe Int
solver1 input = DL.foldl' folder Nothing input
	where
	tree = DL.foldl' (flip btInsert) Empty input
	folder Nothing a
		|  btElem (2020 - a) tree = Just (a * (2020 - a))
		|  otherwise = Nothing
	folder (Just a) _ = Just a


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
			print $ solver1 input
