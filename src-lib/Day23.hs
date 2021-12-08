module Day23 ( runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (digit, anyChar, space, notChar, decimal, string, char, endOfLine)
import qualified Data.List as DL
import EnumerableList (EnumList)
import qualified EnumerableList as EL
import Debug.Trace


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

prevDigit :: Digit -> Digit
prevDigit One = Nine
prevDigit Two = One
prevDigit Three = Two
prevDigit Four = Three
prevDigit Five = Four
prevDigit Six = Five
prevDigit Seven = Six
prevDigit Eight = Seven
prevDigit Nine = Eight

charToDigit :: Parser Digit
charToDigit = do
	c <- anyChar
	if c >= '1' && c <= '9'
		then pure $ toEnum $ fromEnum c - fromEnum '1'
		else mempty

type Input = EnumList Digit

solver :: Input -> Maybe Input
solver input = case EL.uncons input of
	Nothing -> Nothing
	Just (x, xs) -> Just $ (!! 9) $ iter EL.empty 0 x xs (EL.length xs)
	where
	iter :: EnumList Digit -> Int -> Digit -> EnumList Digit -> Int -> [EnumList Digit]
	iter ante lenAnte currentCup post lenPost =
		let
			(picked, newAnte, newPost, newLenAnte, newLenPost) = if lenPost >= 3
				then let (p, r) = EL.splitAt 3 post in (p, ante, r, lenAnte, lenPost - 3)
				else let (p, r) = EL.splitAt (3 - lenPost) ante in (post `EL.app` p, r, EL.empty, lenAnte - 3 + lenPost, 0)
			nextCupAux cup
				| cup `EL.elem` picked = nextCupAux (prevDigit cup)
				| otherwise = cup
			nextCup = nextCupAux (prevDigit currentCup)
		in traceShow (picked, newAnte, newPost, newLenAnte,newLenPost, nextCup) $
			case EL.findIndex nextCup newAnte of
			Nothing -> case EL.findIndex nextCup newPost of
				Just idx -> ante `EL.app` -- error $ "Post" ++ show (insert nextCup newPost picked)
				Nothing -> error "F"
			Just idx -> error $ "Ante" ++ show (insert nextCup newAnte picked)

	insert :: Digit -> EnumList Digit -> EnumList Digit -> EnumList Digit
	insert digit dest src = case EL.span (/= digit) dest of
		(f, r) -> case EL.uncons r of
			Nothing -> error "F2"
			Just (x, xs) -> f `EL.app` (x `EL.cons` (src `EL.app` xs))


parseInput :: Parser Input
parseInput = do
	EL.fromList <$> DAB.many1' charToDigit <* endOfLine

runSolution :: FilePath -> IO ()
runSolution filePath = do
	putStrLn "**Day 23**"
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn $ "Error :" ++ err
		Right input -> do
			print input
			print $ solver input
