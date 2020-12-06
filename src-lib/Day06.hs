{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day06  where

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

type Input = String

-- | Generates the solution from the input
-- part 1
solver :: [Input] -> Integer
solver = DL.foldl' (+) 0 . fmap (fromIntegral . length . DL.nub . DL.sort)

-- parseQ :: [String] -> Input
-- parseQ this = undefined

-- | One Integereger per line
parseQ :: [String] -> [Input]
parseQ = fmap doer . DLS.splitWhen DL.null
	where
	doer :: [String] -> Input
	doer (b:bs) = DL.foldl' folder b bs
	folder :: Input -> String -> Input
	folder acc itm = filter (`elem` acc) itm

-- Apparently uou had to find the gap op the seatids
runSolution :: FilePath -> IO ()
runSolution filePath = do
	contents <- readFile filePath
	print $ solver $ parseQ $ lines contents
