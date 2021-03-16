{-# LANGUAGE OverloadedStrings #-}
module Day22 ( runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString ((<?>), Parser)
import qualified Data.Attoparsec.ByteString as DAB
import qualified Data.Attoparsec.ByteString.Char8 as DABC8

import Data.Hashable (Hashable)
import qualified Data.Hashable
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State

import qualified Data.List as DL
import Data.Foldable

import Control.Arrow

import Control.Lens

import Data.Function (on)

import Debug.Trace

type Input = (Deck Int, Deck Int)

-- | A deck
-- toList ( Deck [a,b] [c,d] ) = [a,b,d,c]
data Deck a = Deck [a] [a]
	deriving (Eq, Show)

instance Foldable Deck where
	foldMap f (Deck hs ts) = foldMap f $ hs ++ reverse ts

instance Hashable a => Hashable (Deck a) where
	hashWithSalt salt = Data.Hashable.hashWithSalt salt . toList
	hash = Data.Hashable.hash . toList
	
-- | Create a deck from a list
fromList :: [a] -> Deck a
fromList = flip Deck []

-- | Append to a deck
append :: a -> Deck a -> Deck a
append a (Deck hs ts) = Deck hs (a : ts)

deckSize :: Deck a -> Int
deckSize (Deck hs ts) = ((+) `on` length) hs ts

-- | Try to take n elemens from a list
deckTake :: Int -> [a] -> Maybe [a]
deckTake n = fmap reverse . go [] n
	where
	go acc n []
		| n <= 0 = Just acc
		| otherwise = Nothing
	go acc n (dh : dt)
		| n <= 0 = Just acc
		| otherwise = go (dh : acc) (n - 1) dt

-- | Extract the first element of a deck (unsafe)
pop :: Deck a -> (a, Deck a)
pop (Deck (h : hs) ts) = (h, Deck hs ts)
pop (Deck [] ts) = case reverse ts of
	(t : ts') -> (t, Deck ts' [])
	[] -> error "pop: empty deck"

-- | Solve part 1
solver :: Input -> Maybe Int
solver = fmap (DL.foldl' (+) 0 . zipWith (*) [1..] . reverse . winner) .
		DL.find finished . iterate iterator
	where
	winner (Deck [] [], d) = toList d
	winner (d, Deck [] []) = toList d

	-- This is where "rounds" take place.
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

	finished :: Input -> Bool
	finished (Deck [] [], _) = True
	finished (_, Deck [] []) = True
	finished _ = False

-- Given the decks for player 1 and 2 return the deck of the winner
-- Left: deck of player 1
-- Right: deck of player 2
solver2 :: (Deck Int, Deck Int) -> Int
solver2 i = DL.foldl' (+) 0 $ zipWith (*) [1..] $ reverse $ toList $ fromEither $ State.evalState (iteration i) HashSet.empty
	where
	iteration :: (Deck Int, Deck Int) -> State (HashSet ([Int], [Int])) (Either (Deck Int) (Deck Int))
	iteration (Deck [] [], d2) = pure $ Right d2
	iteration (d1, Deck [] []) = pure $ Left d1
	iteration initial = do
		let (l1@(h1 : t1), l2@(h2 : t2)) = (toList *** toList) initial
		visited <- HashSet.member (l1, l2) <$> State.get
		if visited
			-- If the configuration has been visited then player 1 wins
			then pure $ Left (fst initial)
			else traceShow initial $ do
				-- save new configuration
				State.modify (HashSet.insert (l1, l2))

				-- pop top cards
				-- let (h1, deck1') = pop $ fst initial
				-- let (h2, deck2') = pop $ snd initial

				-- let (mn, mx) = (min h1 h2, max h1 h2)

				case (deckTake h1 t1, deckTake h2 t2) of
					-- recursive combat
					(Just nd1, Just nd2) -> trace "Subgame" $ case State.evalState (iteration (Deck nd1 [], Deck nd2 [])) HashSet.empty of
						Left _ -> trace "Won 1" $ iteration (Deck t1 [h2, h1] , Deck t2 [])
						Right _ -> trace "Won 2" $ iteration ( Deck t1 [] , Deck t2 [h1, h2])

					-- normal combat
					_ |  h1 > h2 -> iteration ( Deck t1 [h2, h1] , Deck t2 [])
					_ -> iteration ( Deck t1 [] , Deck t2 [h1, h2])

	unfinished :: (Deck Int, Deck Int) -> Bool
	unfinished (Deck [] [], _) = False
	unfinished (_, Deck [] []) = False
	unfinished _ = True

	fromEither :: Either a a -> a
	fromEither (Left a) = a
	fromEither (Right a) = a

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
			print $ solver2 input
