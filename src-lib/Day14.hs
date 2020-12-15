{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Day14 where

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

import qualified Control.Foldl as L

import Data.Bifunctor (bimap)

import Data.Complex
import Data.Functor

import Data.Word
import Data.Bits

type Mask = Vector Char
newtype Instruction = Instruction (Int, Int) deriving Show --address value

type Input = Either Mask Instruction

data MState = MState
	{ _memory :: Map Int Int
	, _mask :: Mask } deriving Show
makeLenses ''MState

solver input =  case input of
		(Left initiaLMask:rest) -> L.fold L.sum $ Map.elems $ (^. memory) $
			DL.foldl' folder (MState Map.empty initiaLMask) rest
	where
	folder :: MState -> Either Mask Instruction -> MState
	folder acc (Left newMask) = (mask .~ newMask) acc
	folder acc (Right (Instruction (addr, val))) = (memory %~ modder (applyMask (acc ^. mask) val)) acc
		where
		modder :: Int -> Map Int Int -> Map Int Int
		modder val = Map.insert addr val

applyMask :: Mask -> Int -> Int
applyMask mask org = L.fold (L.prefilter ((/='X') . snd) folder) $
	zip [0..] $ reverse $ Vec.toList mask
	where
	folder :: L.Fold (Int, Char) Int
	folder = L.Fold realFolder org id
	realFolder :: Int -> (Int, Char) -> Int
	realFolder v (pos, '1') = setBit v pos
	realFolder v (pos, '0') = clearBit v pos

--applyMask' :: Mask -> Int -> [Int]
applyMask' mask org = DLS.splitOn "X" $ Vec.toList mask
	--DL.foldr folder $
--	zip [0..] $ reverse $ Vec.toList mask
--	where
--	folder :: L.Fold (Int, Char) [Int]
--	folder = L.Fold realFolder org id
--	realFolder :: [Int] -> (Int, Char) -> [Int]
--	realFolder v (pos, '1') = setBit v pos
--	realFolder v (pos, '0') = clearBit v pos


parseMask :: Parser Mask
parseMask =
	( (DAB.string "mask = " DAB.<?> "mask =") *>
	DAB.count 36 (DAB.choice (char <$> ['0','1','X']))
	<* endOfLine) <&> Vec.fromList

parseInst :: Parser Instruction
parseInst = curry Instruction  <$>
	(DAB.string "mem[" *> decimal <* DAB.string "] = ") <*>
	decimal <* endOfLine

parseInput :: Parser [Input]
parseInput =
	DAB.many1' (DAB.eitherP parseMask parseInst)

parseQ :: [String] -> Vector Int
parseQ = Vec.fromList . fmap read

runSolution :: FilePath -> IO ()
runSolution filePath = do
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn $ "Error :" ++ err
		Right input -> do
			print $ applyMask (Vec.fromList "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X") 11
			print $ applyMask (Vec.fromList "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X") 101
			print $ applyMask (Vec.fromList "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X") 0
			print $ solver input
