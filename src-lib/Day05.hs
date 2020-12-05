{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day05  where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (inClass, digit, endOfLine, char, anyChar, letter_ascii)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.List as DL
import qualified Data.List.Split as DLS

import qualified Data.Map.Strict as Map

import Debug.Trace

import Data.Maybe (isJust)

-- 128 rows
-- 8 columns

rows :: Integer
rows = 128

cols :: Integer
cols = 8

log2 :: Floating a => a -> a
log2 = logBase 2

seatid :: Integer -> Integer -> Integer
seatid row col = row * 8 + col

type Input = String

-- | Generates the solution from the input
-- part 1
solver :: [Input] -> [(Integer, Integer)]
solver input = 
	filter (\x@(r, c) -> (r /= 0) && (r /= 128) && x `notElem` usedSeats) allSeats
	where
	usedSeats :: [(Integer, Integer)]
	usedSeats = fmap (traceShowId) $ DL.sort $ fmap seatLocation input
	allSeats :: [(Integer, Integer)]
	allSeats = (,) <$> [0..rows-1] <*> [0..cols-1]

solver' :: [Input] -> Integer
solver' = DL.maximum . fmap (uncurry seatid . seatLocation)

seatLocation :: Input -> (Integer, Integer)
seatLocation = go (rows `div` 2) (cols `div` 2)
	where
	go :: Integer -> Integer -> String -> (Integer, Integer)
	go rStep@(0) cStep@(0) [] = (0, 0)
	go rStep cStep ('F':xs) = (0, 0) `sumVec` go (rStep `div` 2) cStep xs
	go rStep cStep ('B':xs) = (rStep, 0) `sumVec` go (rStep `div` 2) cStep xs
	go rStep@(0) cStep ('L':xs) = (0, 0) `sumVec` go rStep (cStep `div` 2) xs
	go rStep@(0) cStep ('R':xs) = (0, cStep) `sumVec` go rStep (cStep `div` 2) xs

sumVec :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
sumVec (a, b) (c, d) = (a+c, b+d)


parsePass :: [String] -> Input
parsePass this = undefined

-- | One Integereger per line
parseInput :: [String] -> [Input]
parseInput = fmap parsePass . DLS.splitWhen DL.null

-- Apparently uou had to find the gap op the seatids
runSolution :: FilePath -> IO ()
runSolution filePath = do
	contents <- readFile filePath
	print $ solver $ lines contents
	print (seatid 75 3)
	print (seatid 1 0)
	print (seatid 1 1)
