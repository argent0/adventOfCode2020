{-# LANGUAGE ScopedTypeVariables #-}

-- | A list of a bounded enumerable type

module EnumerableList where

import Data.Proxy (Proxy(..))
import Data.List (foldl')


-- 0 can't be used because it's impossible to distinguish 0x from 00x with this
-- representation.
newtype EnumList a = EnumList Integer

instance (Enum a, Bounded a, Show a) => Show (EnumList a) where
	show = ("fromList " ++) . show . reverse . toList

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
empty = EnumList 0

singleton :: forall a . (Enum a, Bounded a) => a -> EnumList a
singleton = EnumList . fromIntegral . (+1) . fromEnum

isNull :: forall a . (Enum a, Bounded a) => EnumList a -> Bool
isNull (EnumList ds)
	| ds <= 0 = True
	| otherwise = False

-- Cons a digit into a list of digits (aka integer)
-- O(1)
cons :: forall a . (Enum a, Bounded a) => a -> EnumList a -> EnumList a
cons d l@(EnumList n)
	| isNull l = singleton d
	| otherwise =
	EnumList $ n * fromIntegral (nSymbols (Proxy :: Proxy a)) + fromIntegral (1 + fromEnum d)

fromList :: forall a . (Enum a, Bounded a) => [a] -> EnumList a
fromList = foldl' (flip cons) empty

-- index
-- O(1)
index :: forall a . (Enum a, Bounded a) => EnumList a -> Int -> Maybe a
index (EnumList n) idx
	| n <= 0 = Nothing
	| n >= (fromIntegral (nSymbols (Proxy :: Proxy a)) ^ fromIntegral idx) - 1 =
		Just $ toEnum $ (+ (-1)) $ fromInteger $ ( n `div` base) `mod` (base - 1)
	| otherwise = Nothing
	where
	base :: Integer
	base = fromIntegral $ nSymbols (Proxy :: Proxy a) + 2

-- take
-- O(1)
take :: forall a . (Enum a, Bounded a) => Int -> EnumList a -> EnumList a
take n l@(EnumList ds)
	| isNull l || n <= 0 = empty
	| otherwise = EnumList (ds `mod` (fromIntegral (nSymbols (Proxy :: Proxy a)) ^ fromIntegral n))

-- split
-- O(1)
splitAt :: forall a . (Enum a, Bounded a) =>
	Int -> EnumList a -> (EnumList a, EnumList a)
splitAt n org@(EnumList ds)
	| isNull org = (empty, empty)
	| n <= 0 = (empty, org)
	| otherwise =
		let (t, h) = ds `divMod` (fromIntegral (nSymbols (Proxy :: Proxy a)) ^ fromIntegral n) in
		(EnumList h, EnumList t)

elem :: forall a . (Enum a, Bounded a) => a -> EnumList a -> Bool
elem d l@(EnumList ds)
	| isNull l = False
	| otherwise =
		let (t, h) = ds `divMod` fromIntegral (nSymbols (Proxy :: Proxy a)) in
		(fromEnum d == (fromInteger h - 1)) || t > 0 && EnumerableList.elem d (EnumList t)

-- | Recover the list of values
toList :: forall a . (Enum a, Bounded a) => EnumList a -> [a]
toList l@(EnumList ds)
	| isNull l = []
	| otherwise =
		let (t, h) = ds `divMod` fromIntegral (nSymbols (Proxy :: Proxy a)) in
		if h == 0
			then []
			else toEnum (fromInteger h - 1) : toList (EnumList t :: EnumList a)

-- span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
scan :: forall a . (Enum a, Bounded a) => (a -> Bool) -> EnumList a -> (EnumList a, EnumList a)
scan p = go empty
	where
	go :: EnumList a -> EnumList a -> (EnumList a, EnumList a)
	go (EnumList acc) l@(EnumList ds)
		| isNull l = (empty, EnumList acc)
		| otherwise = let (t, h) = ds `divMod` fromIntegral (nSymbols (Proxy :: Proxy a)) in
			if p $ toEnum (fromInteger h - 1)
				then go (EnumList $ acc * base + h) (EnumList t)
				else (EnumList acc, EnumList t)

	base :: Integer
	base = fromIntegral $ nSymbols (Proxy :: Proxy a) + 2

-- | break id [False, True, False] = ([False], [True, False])
-- break :: forall a . (Enum a, Bounded a) => (a -> Bool) -> EnumList a -> (EnumList a, EnumList a)
-- break p l@(EnumList ds)
-- 	| isNull l = (empty, empty)
-- 	| otherwise = go empty ds
-- 	where
-- 	go :: EnumList a -> Integer -> (EnumList a, EnumList a)
-- 	go (EnumList acc) dss
-- 		| isNull l = let (t, h) = dss `divMod` fromIntegral (nSymbols (Proxy :: Proxy a)) in
-- 			if not (p $ toEnum (fromInteger h - 1))
-- 				then go (EnumList h) t
-- 				else (empty, EnumList dss)
-- 		| otherwise = let (t, h) = dss `divMod` fromIntegral (nSymbols (Proxy :: Proxy a)) in
-- 			if not (p $ toEnum (fromInteger h - 1))
-- 				then go (EnumList (h +  acc * fromIntegral (nSymbols (Proxy :: Proxy a)))) t
-- 				else (l, EnumList dss)

