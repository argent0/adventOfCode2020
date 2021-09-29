{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A list of a bounded enumerable type

module EnumerableList where

import Data.Proxy (Proxy(..))
import Data.List (foldl')
import Prelude hiding
	( take
	, span
	, splitAt
	, break
	, null
	, map
	, foldl
	, foldr
	, and)
import Debug.Trace
import Data.Maybe (fromMaybe)


-- 0 can't be used because it's impossible to distinguish 0x from 00x with this
-- representation.
newtype EnumList a = EnumList Integer

instance (Enum a, Bounded a, Show a) => Show (EnumList a) where
	show = ("fromList " ++) . show . toList


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
enumListBase :: forall a . (Enum a, Bounded a) => Proxy a -> Int
enumListBase _ = fromEnum (maxBound :: a) + 2

-- Constructors

empty :: forall a . EnumList a
empty = EnumList 0

singleton :: forall a . (Enum a, Bounded a) => a -> EnumList a
singleton = EnumList . fromIntegral . (+1) . fromEnum

-- Cons a digit into a list of digits (aka integer)
-- O(1)
cons :: forall a . (Enum a, Bounded a) => a -> EnumList a -> EnumList a
cons d l@(EnumList n)
	| null l = singleton d
	| otherwise =
	EnumList $ n * fromIntegral base + fromIntegral (1 + fromEnum d)
	where
	base :: Integer
	base = fromIntegral $ enumListBase (Proxy :: Proxy a)

fromList :: forall a . (Enum a, Bounded a) => [a] -> EnumList a
fromList = foldl' (flip cons) empty . reverse

-- Basic Functions

uncons :: forall a . (Enum a, Bounded a) => EnumList a -> Maybe (a, EnumList a)
uncons (EnumList ds) = let (t, h) = ds `divMod` base in
	if h == 0
		then Nothing
		else Just (toEnum $ (+ (-1)) $ fromInteger h, EnumList t)
	where
	base :: Integer
	base = fromIntegral $ enumListBase (Proxy :: Proxy a)

null :: forall a . (Enum a, Bounded a) => EnumList a -> Bool
null (EnumList ds)
	| ds <= 0 = True
	| otherwise = False

-- List Trasformations

map :: forall a b . (Enum a, Bounded a, Enum b, Bounded b) =>
	(a -> b) -> EnumList a -> EnumList b
map f = go empty
	where
	go acc (EnumList ds)
		| ds <= 0 = acc
		| otherwise = let (t, h) = ds `divMod` baseA in
			go (cons (f $ toEnum $ fromIntegral $ h - 1) acc) (EnumList t)

	baseA :: Integer
	baseA = fromIntegral $ enumListBase (Proxy :: Proxy a)

-- Folds

foldl :: (Enum a, Bounded a) => (b -> a -> b) -> b -> EnumList a -> b
foldl f z l = case uncons l of
	Nothing -> z
	Just (h, t) -> (z `f` h) `foldlf` t
	where
	foldlf = foldl f

foldr :: (Enum a, Bounded a) => (a -> b -> b) -> b -> EnumList a -> b
foldr f z l = case uncons l of
	Nothing -> z
	Just (h, t) -> h `f` foldr f z t

-- Special folds

and :: EnumList Bool -> Bool
and = foldr (&&) True


-- | Recover the list of values
-- O(n)
toList :: forall a . (Enum a, Bounded a) => EnumList a -> [a]
toList = reverse . go []
	where
	go acc el = case uncons el of
		Nothing -> acc
		Just (h, t) -> go (h : acc) t

-- Sublists

-- take
-- O(1)
take :: forall a . (Enum a, Bounded a) => Int -> EnumList a -> EnumList a
take n l@(EnumList ds)
	| null l || n <= 0 = empty
	| otherwise = EnumList (ds `mod` (fromIntegral (enumListBase (Proxy :: Proxy a)) ^ fromIntegral n))

-- split
-- O(1)
splitAt :: forall a . (Enum a, Bounded a) =>
	Int -> EnumList a -> (EnumList a, EnumList a)
splitAt n org@(EnumList ds)
	| null org = (empty, empty)
	| n <= 0 = (empty, org)
	| otherwise =
		let (t, h) = ds `divMod` (fromIntegral (enumListBase (Proxy :: Proxy a)) ^ fromIntegral n) in
		(EnumList h, EnumList t)

elem :: forall a . (Enum a, Bounded a) => a -> EnumList a -> Bool
elem d l@(EnumList ds)
	| null l = False
	| otherwise =
		let (t, h) = ds `divMod` fromIntegral (enumListBase (Proxy :: Proxy a)) in
		(fromEnum d == (fromInteger h - 1)) || t > 0 && EnumerableList.elem d (EnumList t)


-- span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
span :: forall a . (Enum a, Bounded a) => (a -> Bool) -> EnumList a -> (EnumList a, EnumList a)
span p = go empty
	where
	go :: EnumList a -> EnumList a -> (EnumList a, EnumList a)
	go (EnumList acc) l@(EnumList ds)
		| null l = (empty, EnumList acc)
		| otherwise = let (t, h) = ds `divMod` fromIntegral (enumListBase (Proxy :: Proxy a)) in
			if p $ toEnum (fromInteger h - 1)
				then go (EnumList $ acc * base + h) (EnumList t)
				else (EnumList acc, EnumList (t*base + h))

	base :: Integer
	base = fromIntegral $ enumListBase (Proxy :: Proxy a)

-- | break id [False, True, False] = ([False], [True, False])
break :: forall a . (Enum a, Bounded a) => (a -> Bool) -> EnumList a -> (EnumList a, EnumList a)
break p = span (not . p)

-- Indexing lists

-- index
-- O(1)
index :: forall a . (Enum a, Bounded a) => EnumList a -> Int -> Maybe a
index (EnumList n) idx
	| idx < 0 = error "Negative index"
	| n <= 0 = Nothing
	| n >= (fromIntegral base ^ fromIntegral idx) - 1 =
		Just $ toEnum $ (+ (-1)) $ fromInteger $ ( n `div` (base ^ idx)) `mod` base
	| otherwise = Nothing
	where
	base :: Integer
	base = fromIntegral $ enumListBase (Proxy :: Proxy a)
