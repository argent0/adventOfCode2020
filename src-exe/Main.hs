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
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
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
	["11"] -> Day11.runSolution "input/Day11"
	["12"] -> Day12.runSolution "input/Day12"
	["13"] -> Day13.runSolution "input/Day13"
	["14"] -> Day14.runSolution "input/Day14"
	["15"] -> Day15.runSolution "input/Day15"
	["16"] -> Day16.runSolution "input/Day16"
	["17"] -> Day17.runSolution "input/Day17"
	["18"] -> Day18.runSolution "input/Day18"
	["19"] -> Day19.runSolution "input/Day19"
	["20"] -> Day20.runSolution "input/Day20"
	["21"] -> Day21.runSolution "input/Day21"
	["21i"] -> Day21.runSolution "short-input/Day21"
	["22"] -> Day22.runSolution "input/Day22"
	["22i"] -> Day22.runSolution "short-input/Day22"
	["23"] -> Day23.runSolution "input/Day23"
	["24"] -> Day24.runSolution "input/Day24"
	args -> error $ "Args were: " ++ show args
