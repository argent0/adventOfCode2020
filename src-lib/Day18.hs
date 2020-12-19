{-# LANGUAGE OverloadedStrings #-}
module Day18 ( runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (anyChar, space, notChar, decimal, string, char, endOfLine)
import qualified Data.Attoparsec.ByteString.Char8 as DABC8

import Data.Functor
import Debug.Trace
import Control.Arrow
import qualified Control.Foldl as L
import qualified Data.List as DL

data Expr a = Literal a
	| Sum (Expr a) (Expr a)
	| Prod (Expr a) (Expr a)
	| Paren (Expr a)
	deriving Show

type Input = Expr Integer

eval2 :: Num a => Expr a -> a
eval2 (Literal n) = n
eval2 (Sum a b) = eval2 a + eval2 b
eval2 (Prod a b) = eval2 a * eval2 b
eval2 (Paren e) = eval2 e

eval :: Num a => Expr a -> a
eval (Literal n) = n
eval (Sum a b) = evalGo (eval a +) b
eval (Prod a b) = evalGo (eval a *) b
eval (Paren e) = eval e

evalGo :: Num a => (a -> a) -> Expr a -> a
evalGo f (Literal n) = f n
evalGo f (Sum a b) = evalGo ((f $ eval a) +) b
evalGo f (Prod a b) = evalGo ((f $ eval a) *) b
evalGo f (Paren e) = f $ eval e

solver input = input

-- Part 1
--parseInput :: Parser [Input]
--parseInput = DAB.sepBy1' parseExpr endOfLine
--	where
--	parseExpr :: Parser (Expr Integer)
--	parseExpr = DAB.choice
--		[ do
--			start <- DAB.choice [parens, Literal <$> decimal]
--			_ <- space
--			op <- DAB.choice $ fmap char "*+"
--			_ <- space
--			case op of
--				'+' -> Sum start <$> parseExpr
--				'*' -> Prod start <$> parseExpr
--				
--		, parens
--		, Literal <$> decimal
--		]
--	parens :: Parser (Expr Integer)
--	parens = Paren <$> ( char '(' *> parseExpr <* char ')' )

-- Part 2
parseInput :: Parser [Input]
parseInput = DAB.sepBy1' parseExpr endOfLine
	where
	parseExpr :: Parser (Expr Integer)
	parseExpr = DAB.choice
		[ do
			factors <- DAB.sepBy1' (DAB.choice [aSum, parens, Literal <$> decimal]) (string " * ")
			pure $ DL.foldl' Prod (Literal 1) factors
		, parens
		, Literal <$> decimal
		]
	aSum =
		DL.foldl' folder (Literal 0) <$> DAB.sepBy1' (DAB.choice [Literal <$> decimal, parens]) (string " + ")
	folder :: Expr Integer -> Expr Integer -> Expr Integer
	folder acc n = Sum acc n
	parens :: Parser (Expr Integer)
	parens = Paren <$> ( char '(' *> parseExpr <* char ')' )

runSolution :: FilePath -> IO ()
runSolution filePath = do
	putStrLn "**Day 18**"
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn $ "Error :" ++ err
		Right input -> do
			print $ L.fold L.sum $ fmap eval input
			print $ L.fold L.sum $ fmap eval2 input
