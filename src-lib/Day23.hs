{-# LANGUAGE ScopedTypeVariables #-}
module Day23 ( runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (digit, anyChar, space, notChar, decimal, string, char, endOfLine)
import qualified Data.List as DL

import Data.Proxy (Proxy(..))

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

-- | A list of a bounded enumerable type
--
-- 0 can't be used because it's impossible to distinguish 0x from 00x with this
-- representation.
newtype EnumList a = EnumList (Maybe Integer)
	deriving Show

-- | The "base" used to pack the values into an integer.
--
-- fromEnum False = 0
-- fromEnum True = 1
--
-- maxBound :: Bool = True
--
-- So I would need base of 3 to store a bool.
--
-- This could also be called a "HashList"
nSymbols :: forall a . (Enum a, Bounded a) => Proxy a -> Int
nSymbols _ = fromEnum (maxBound :: a) + 2

empty :: forall a . EnumList a
empty = EnumList Nothing

singleton :: forall a . (Enum a, Bounded a) => a -> EnumList a
singleton = EnumList . Just . fromIntegral . (+1) . fromEnum

-- Cons a digit into a list of digits (aka integer)
-- O(1)
iCons :: forall a . (Enum a, Bounded a) => a -> EnumList a -> EnumList a
iCons d (EnumList Nothing) = singleton d
iCons d (EnumList (Just ds)) =
	EnumList $ Just $ ds * fromIntegral (nSymbols (Proxy :: Proxy a)) + fromIntegral (1 + fromEnum d)

-- index
-- O(1)
iIndex :: forall a . (Enum a, Bounded a) => EnumList a -> Int -> Maybe a
iIndex (EnumList Nothing) _ = Nothing
iIndex (EnumList (Just ds)) idx
	| ds >= (fromIntegral (nSymbols (Proxy :: Proxy a)) ^ fromIntegral idx) - 1 =
		Just $ toEnum $ (+ (-1)) $ fromInteger $ ( ds `div` (fromIntegral (fromEnum (maxBound :: a) + 2) ^ fromIntegral idx)) `mod` fromIntegral (fromEnum (maxBound :: a) + 1)
	| otherwise = Nothing

-- take
-- O(1)
iTake :: forall a . (Enum a, Bounded a) => Int -> EnumList a -> EnumList a
iTake _ ds@(EnumList Nothing) = ds
iTake n (EnumList (Just ds))
	| n <= 0 = EnumList Nothing
	| otherwise = EnumList (Just $ ds `mod` (fromIntegral (nSymbols (Proxy :: Proxy a)) ^ fromIntegral n))

-- split
-- O(1)
iSplitAt :: forall a . (Enum a, Bounded a) =>
	Int -> EnumList a -> (EnumList a, EnumList a)
iSplitAt _ ds@(EnumList Nothing) = (ds, ds)
iSplitAt n (EnumList (Just ds))
	| n <= 0 = (EnumList Nothing, EnumList (Just ds))
	| otherwise =
		let (t, h) = ds `divMod` (fromIntegral (nSymbols (Proxy :: Proxy a)) ^ fromIntegral n) in
		(EnumList (Just h), EnumList (Just t))

iElem :: forall a . (Enum a, Bounded a) => a -> EnumList a -> Bool
iElem d (EnumList Nothing) = False
iElem d (EnumList (Just ds)) = 
	let (t, h) = ds `divMod` fromIntegral (nSymbols (Proxy :: Proxy a)) in
	(fromEnum d == (fromInteger h - 1)) || t > 0 && iElem d (EnumList (Just t))

-- | Recover the list of values
iUnfold :: forall a . (Enum a, Bounded a) => EnumList a -> [a]
iUnfold (EnumList Nothing) = []
iUnfold (EnumList (Just ds)) =
	let (t, h) = ds `divMod` fromIntegral (nSymbols (Proxy :: Proxy a)) in
	if h == 0
		then []
		else toEnum (fromInteger h - 1) : iUnfold (EnumList (Just t) :: EnumList a)

iBreak :: forall a . (Enum a, Bounded a) => (a -> Bool) -> EnumList a -> (EnumList a, EnumList a)
iBreak _ (EnumList Nothing) = (EnumList Nothing, EnumList Nothing)
iBreak p (EnumList (Just ds)) = go (EnumList Nothing) ds
	where
	go :: EnumList a -> Integer -> (EnumList a, EnumList a)
	go (EnumList Nothing) dss =
		let (t, h) = dss `divMod` fromIntegral (nSymbols (Proxy :: Proxy a)) in
			if not (p $ toEnum (fromInteger h - 1))
				then go (EnumList (Just h)) t
				else (EnumList Nothing, EnumList (Just dss))
	go l@(EnumList (Just acc)) dss =
		let (t, h) = dss `divMod` fromIntegral (nSymbols (Proxy :: Proxy a)) in
			if not (p $ toEnum (fromInteger h - 1))
				then go (EnumList (Just (h +  acc * fromIntegral (nSymbols (Proxy :: Proxy a))))) t
				else (l, EnumList (Just dss))


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
