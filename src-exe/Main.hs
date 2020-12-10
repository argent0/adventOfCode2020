{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified System.Environment as SE

main :: IO ()
main = SE.getArgs >>= \case
	["01"] -> Day01.runSolution "input/Day01"
	["02"] -> Day02.runSolution "input/Day02"
	["03"] -> Day03.runSolution "input/Day03"
	["04"] -> Day04.runSolution "input/Day04"
	["05"] -> Day05.runSolution "input/Day05"
	["06"] -> Day06.runSolution "input/Day06"
	["07"] -> Day07.runSolution "input/Day07"
	["08"] -> Day08.runSolution "input/Day08"
	["09"] -> Day09.runSolution "input/Day09"
	["10"] -> Day10.runSolution "input/Day10"
	args -> error $ "Args were: " ++ show args
