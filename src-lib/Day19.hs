{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Day19 ( runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (anyChar, space, notChar, decimal, string, char, endOfLine)
import qualified Data.Attoparsec.ByteString.Char8 as DABC8
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.List as DL
import Debug.Trace
import Control.Arrow

data Rule
	= Concat Rule Rule
	| Union Rule Rule
	| Terminal Char
	| NonTerminal Int
	deriving (Show, Eq)

data Rule'
	= Concat' Int Int [Int]
	| Union' Rule' Rule'
	| Terminal' Char
	| NonTerminal' Int
	deriving (Show, Eq)

data NDPAStates = P0 | P1 deriving (Show, Eq)

data Output = T Char | NT Int deriving (Show, Eq)

yield :: Vector Rule' -> String -> [Output] -> [(String, [Output])]
--yield rules ss [] = [(ss, [NT 0])]
yield rules ss (NT a:os) = case rules ! a of
	Union' (Concat' r11 r12 r1s) (Concat' r21 r22 r2s) ->
		[ (ss, fmap NT (r11:r12:r1s) ++ os)
		, (ss, fmap NT (r21:r22:r2s) ++ os) ]
	Union' (NonTerminal' r1) (NonTerminal' r2) ->
		[ (ss, NT r1:os)
		, (ss, NT r2:os) ]
	Union' (NonTerminal' r1) (Concat' r21 r22 r2s) ->
		[ (ss, NT r1:os)
		, (ss, fmap NT (r21:r22:r2s) ++ os) ]
	Concat' r1 r2 rs -> [ (ss, fmap NT (r1:r2:rs) ++ os) ]
	Terminal' c -> [(ss, T c:os)]
	NonTerminal' n -> [(ss, NT n:os)]
	un -> error $ show (a, un)

yield rules [] (T _:os) = []

yield rules (s:ss) (T g:os)
	| s == g = [(ss, os)]
	| otherwise = []

yield _ _ [] = []

type Input = Integer

solver input = input

parseRules :: Parser (Vector Rule)
parseRules = V.fromList . fmap snd . DL.sortOn fst <$> DAB.sepBy1' parseRule endOfLine

parseRule :: Parser (Integer, Rule)
parseRule = (,) <$> (decimal <* string ": ") <*> parseUnion
parseUnion = DAB.sepBy1' parseConcat (string " | ") >>= \case
	[r] -> pure r
	(r:rs) -> pure $! DL.foldl' Union r rs
parseConcat = DAB.sepBy1' (DAB.choice [NonTerminal <$> decimal, parseLiteral]) (char ' ') >>= \case
	[r] -> pure r
	(r:rs) -> pure $ DL.foldl Concat r rs
parseLiteral = Terminal <$> (char '"' *> notChar '"' <* char '"')

parseRules' :: Parser (Vector Rule')
parseRules' = V.fromList . fmap snd . DL.sortOn fst <$> DAB.sepBy1' parseRule' endOfLine

parseRule' :: Parser (Integer, Rule')
parseRule' = (,) <$> (decimal <* string ": ") <*> DAB.choice [parseLiteral', parseUnion']
parseUnion' = DAB.sepBy1' parseConcat' (string " | ") >>= \case
	[r] -> pure r
	[r1, r2] -> pure $ Union' r1 r2
	o -> error $ "Union with more than 2" ++ show o
parseConcat' = DAB.sepBy1' decimal (char ' ') >>= \case
	[r] -> pure $ NonTerminal' r
	(r1:r2:rs) -> pure $ Concat' r1 r2 rs
	_ -> error "Union with less than 2"
parseLiteral' = Terminal' <$> (char '"' *> notChar '"' <* char '"')

parseInput :: Parser (Vector Rule, [String])
parseInput = 
	(,) <$>
	(parseRules <* endOfLine <* endOfLine) <*>
	DAB.sepBy1' (DAB.many1' (notChar '\n')) endOfLine

parseInput' :: Parser (Vector Rule', [String])
parseInput' = 
	(,) <$>
	(parseRules' <* endOfLine <* endOfLine) <*>
	DAB.sepBy1' (DAB.many1' (notChar '\n')) endOfLine

runSolution :: FilePath -> IO ()
runSolution filePath = do
	putStrLn "**Day 19**"
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput' contents
	case parseResult of
		Left err -> putStrLn $ "Error :" ++ err
		Right (rules, samples) -> do
			--putStrLn $ unlines $ fmap (show . (nonDet 1000 rules &&& id)) samples
			print $ DL.foldl' (+) 0 $ fmap (fromEnum . nonDet 1000 rules) samples

			-- 8: 42 | 42 8
			-- 11: 42 31 | 42 11 31

			let alternateRules = rules V.// 
				[ (8, Union' (NonTerminal' 42) (Concat' 42 8 []))
				, (11, Union' (Concat' 42 31 []) (Concat' 42 11 [31]))]

			--putStrLn $ unlines $ fmap (show . (nonDet 1000 alternateRules &&& id)) $ samples
			print $ DL.foldl' (+) 0 $ fmap (fromEnum . nonDet 1000 alternateRules) samples

nonDet :: Int -> Vector Rule' -> String-> Bool
nonDet maxIter rules input = or $ go maxIter input [NT 0]
	where
	go :: Int -> String -> [Output] -> [Bool]
	go 0 i o = error $ "Max iterations reached: " ++ input ++ ", " ++ i ++ ", " ++ show o
	go n i o = --traceShow (i, o) $
		do
			(ni, no) <- yield rules i o
			if ([], []) == (ni, no)
				then pure True
				else go (n - 1) ni no


-- Right
-- 265
-- 394
