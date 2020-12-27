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
import Data.Attoparsec.ByteString.Char8 (decimal, endOfLine, char)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as DL
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Control.Monad as CM
import qualified Control.Foldl as L
import Data.Bits


-- | A string of 1,0 or X
--
-- mask = 111101X010011110100100110100101X0X0X
type Mask = Vector Char

-- | An instruction writes to memory
--
-- mem[2629] = 12036738
newtype Instruction = Instruction (Int, Int) deriving Show --address value

-- | Each input line either sets the mask or writes to memory
type Input = Either Mask Instruction

-- | The state of the virtual machine
data MState = MState
	{ _memory :: Map Int Int
	, _mask :: Mask } deriving Show
makeLenses ''MState

-- | Solve part 1
--
-- It only makes sence if the first instruction sets the mask value.
solver :: [Input] -> Maybe Int
solver input =  case input of
		(Left initiaLMask:rest) -> Just $
			-- Sum all the values in memory
			L.fold L.sum $ Map.elems $ (^. memory) $
			-- Interpret all instructions.
			DL.foldl' folder (MState Map.empty initiaLMask) rest
		_ -> Nothing
	where
	folder :: MState -> Either Mask Instruction -> MState
	folder acc (Left newMask) = (mask .~ newMask) acc
	folder acc (Right (Instruction (addr, val))) = (memory %~ modder (applyMask (acc ^. mask) val)) acc
		where
		-- Writes the resulting value
		modder :: Int -> Map Int Int -> Map Int Int
		modder val = Map.insert addr val

-- | Solve part 2
--
-- See coments in part 1
solver2 :: [Input] -> Maybe Int
solver2 input =  case input of
		(Left initiaLMask:rest) -> Just $
			L.fold L.sum $ Map.elems $ (^. memory) $
			DL.foldl' folder (MState Map.empty initiaLMask) rest
		_ -> Nothing
	where
	folder :: MState -> Either Mask Instruction -> MState
	folder acc (Left newMask) = (mask .~ newMask) acc
	folder acc (Right (Instruction (addr, val))) = (memory %~ modder (acc ^. mask)) acc
		where
		-- Writes in all resulting addresses
		modder :: Mask -> Map Int Int -> Map Int Int
		modder mask mem = DL.foldl' (\m a -> Map.insert a val m) mem $ applyMask2 mask addr

-- | Apply the mask in part 1
--
-- 0 in mask clears the bit
-- 1 in mask sets the bit
-- X in mask leaves the bit unchanged
applyMask :: Mask -> Int -> Int
applyMask mask org =
	DL.foldl' folder org $
	filter ((/= 'X') . snd) $
	-- index each char with its position
	zip [0..] $ reverse $ Vec.toList mask
	where
	folder :: Int -> (Int, Char) -> Int
	folder v (pos, '1') = setBit v pos
	folder v (pos, '0') = clearBit v pos

-- | Apply the mask in part 2
--
-- 0 in mask leaves the bit unchanged
-- 1 in mask sets the bit
-- X branches into two posibilities where the bit at that position is 0 or 1.
applyMask2 :: Mask -> Int -> [Int]
applyMask2 mask org = expandMask $
	-- index each char with its position
	zip [0..] $ reverse $ Vec.toList mask
	where
	-- Use the list monad
	expandMask :: [(Int, Char)] -> [Int]
	expandMask = CM.foldM folder org
	folder :: Int -> (Int, Char) -> [Int]
	-- There are two options when the char is 'X'
	folder acc (pos, 'X') = [clearBit acc pos, setBit acc pos]
	-- There is only one option other wise
	folder acc (pos, '1') = [setBit acc pos]
	folder acc (pos, '0') = [acc]

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

runSolution :: FilePath -> IO ()
runSolution filePath = do
	putStrLn "**Day 14**"
	contents <- BS.readFile filePath
	let parseResult = DAB.parseOnly parseInput contents
	case parseResult of
		Left err -> putStrLn $ "Error :" ++ err
		Right input -> do
			print $ solver input
			print $ solver2 input
