{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day12 where

import Control.Lens
import Control.Lens.TH

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import qualified Data.Attoparsec.ByteString.Char8 as DABC8
import Data.Attoparsec.ByteString.Char8 (decimal, inClass, digit, endOfLine, char, anyChar, letter_ascii, notChar)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Data.List as DL
import qualified Data.List.Split as DLS

import Debug.Trace

import Control.Arrow

import Data.Array.IArray (Array)
import qualified Data.Array.IArray as IA

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either (isRight)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Bool (bool)

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec

import Data.Function (on)

import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State

import qualified Control.Monad as CM

-- import qualified Control.Foldl as L

import Data.Bifunctor (bimap)

import Data.Complex

--type Input = (IA.Array Int (IA.Array Int Char))
type Input = (Char, Int)

-- >>> solver [('F', 10),

solver input =
	uncurry (+) $
	(abs . realPart &&& abs . imagPart) $ snd $
	DL.foldl' (\x y -> traceShow (x, y) (navigate y x)) (10 :+ 1, 0 :+ 0) input
	where
	navigate :: Input -> (Complex Float, Complex Float) -> (Complex Float, Complex Float)
	navigate ('N', n) = first (+ imag n)
	navigate ('S', n) = first (dec (imag n))
	navigate ('E', n) = first (+ real n)
	navigate ('W', n) = first (dec (real n))

	navigate ('L', 90) = first (* (0 :+ 1))
	navigate ('L', 180) = first $ fmap round' . (* (0 :+ 1) ** 2)
	navigate ('L', 270) = first $ fmap round' . (* (0 :+ 1) ** 3)

	navigate ('R', 90) = first $ fmap round' . (* (0 :+ (-1)))
	navigate ('R', 180) = first $ fmap round' . (* (0 :+ (-1)) ** 2)
	navigate ('R', 270) = first $ fmap round' . (* (0 :+ (-1)) ** 3)
	navigate ('F', n) = uncurry (CM.liftM2 (.) (,) ((+) . (real n *)))

	navigate (c, n) = error $ show (c, n)

	norm :: Complex Float -> Complex Float
	norm x = x / abs x

	real n = fromIntegral n :+ 0
	imag n = 0 :+ fromIntegral n
	dec n x = x - n

	round' :: Float -> Float
	round' = fromIntegral . round

parseInput :: Parser [Input]
parseInput =
	((,) <$> anyChar <*> decimal) `DAB.sepBy1'` endOfLine

runSolution :: FilePath -> IO ()
runSolution filePath = do
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn err
		Right input -> do
			print $ solver input

-- 1526 wrong
-- part ii
-- 1463 wrong
