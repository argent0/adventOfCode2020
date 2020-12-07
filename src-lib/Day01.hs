{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Day01 (runSolution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (digit, endOfLine)

import qualified Data.List as DL
import Data.Semigroup (First(..), Option(..))
import Data.Maybe (listToMaybe)

{-
 - Could use a binnary tree to make this n log n complexity.
 -
 - for every n in the input insert it in BT and search that tree for 2020-n
 -}

-- | Generates the solution from the input
solver2 :: [Integer] -> Maybe Integer
solver2 = fmap getFirst . getOption . foldMap folder . DL.tails
	where
	folder :: [Integer] -> Option (First Integer)
	folder [] = Option Nothing
	folder (x:xs) = Option $ First . (x *) <$> findPairProduct (2020 - x) xs

solver1 :: [Integer] -> Maybe Integer
solver1 = findPairProduct 2020

-- | Find the product of the first  pairs that adds to some total
-- >>> findPairProduct 10 [3, 7, undefined]
-- Just 21
findPairProduct :: Integer -> [Integer] -> Maybe Integer
findPairProduct total = fmap (uncurry (*)) . listToMaybe .
	findPairs (\x y -> x + y == total)

-- | Find all pairs that verify the predicate
--
-- O(n^2)
--
-- >>> findPairs (\x y -> x + y == 10) [3, 2, 8, 6, 4, 7]
-- [(3,7),(2,8),(6,4)]
--
-- >>> findPairs (\x y -> x + y == 10) [5]
-- []
findPairs :: forall a . (a -> a -> Bool) -> [a] -> [(a, a)]
findPairs predicate = foldr folder [] . DL.tails
	where
	folder :: [a] -> [(a, a)] -> [(a, a)]
	folder [] acc = acc
	folder (x:xs) acc = maybe acc ((: acc) . (x,)) $ DL.find (predicate x) xs


parseInput :: Parser [Integer]
parseInput =
	fmap (read :: String -> Integer) . filter (not . null) <$>
		DAB.many' digit `DAB.sepBy` endOfLine

runSolution :: FilePath -> IO ()
runSolution filePath = do
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn err
		Right input ->
			print $ solver1 input
