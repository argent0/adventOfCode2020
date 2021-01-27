{-# LANGUAGE OverloadedStrings #-}
module Day22 ( runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString ((<?>), Parser)
import qualified Data.Attoparsec.ByteString as DAB
import qualified Data.Attoparsec.ByteString.Char8 as DABC8

import qualified Data.List as DL
import Data.Foldable

import Control.Arrow

type Input = (Deck Int, Deck Int)

-- | A deck
-- toList ( Deck [a,b] [c,d] ) = [a,b,d,c]
data Deck a = Deck [a] [a]
	deriving Show

instance Foldable Deck where
	foldMap f (Deck hs ts) = foldMap f $ hs ++ reverse ts
	
-- | Create a deck from a list
fromList :: [a] -> Deck a
fromList = flip Deck []

-- | Append to a deck
append :: a -> Deck a -> Deck a
append a (Deck hs ts) = Deck hs (a : ts)

-- | Extract the first element of a deck (unsafe)
pop :: Deck a -> (a, Deck a)
pop (Deck (h : hs) ts) = (h, Deck hs ts)
pop (Deck [] ts) = case reverse ts of
	(t : ts') -> (t, Deck ts' [])
	[] -> error "pop: empty deck"

-- | Solve part 1
solver :: Input -> Int
solver = DL.foldl' (+) 0 .
	zipWith (*) [1..] . reverse . winner . head . dropWhile unfinished . iterate iterator
	where
	winner (Deck [] [], d) = toList d
	winner (d, Deck [] []) = toList d

	iterator :: Input -> Input
	iterator (deck1, deck2) = let
		(h1, deck1') = pop deck1
		(h2, deck2') = pop deck2
		in if h1 > h2
			then
				( append h2 (append h1 deck1')
				, deck2')
			else
				( deck1'
				, append h1 (append h2 deck2'))

	unfinished :: Input -> Bool
	unfinished (Deck [] [], _) = False
	unfinished (_, Deck [] []) = False
	unfinished _ = True

parseInput :: Parser Input
parseInput = (fromList *** fromList) <$> ( (,) <$>
	( (DABC8.string "Player 1:" <?> "Player 1:") *>
		DABC8.endOfLine *>
		DAB.sepBy1' DABC8.decimal DABC8.endOfLine <*
		DABC8.endOfLine <* DABC8.endOfLine)
	<*>
	( (DABC8.string "Player 2:" <?> "Player 2:") *>
		DABC8.endOfLine *>
		DAB.sepBy1' DABC8.decimal DABC8.endOfLine <*
		DABC8.endOfLine )
	)

runSolution :: FilePath -> IO ()
runSolution filePath = do
	putStrLn "**Day 22**"
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn $ "Error :" ++ err
		Right input -> do
			print $ solver input
