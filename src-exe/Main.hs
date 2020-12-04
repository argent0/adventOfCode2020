{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified System.Environment as SE

main :: IO ()
main = SE.getArgs >>= \case
	["01"] -> Day01.runSolution "input/Day01"
	["02"] -> Day02.runSolution "input/Day02"
	["03"] -> Day03.runSolution "input/Day03"
	["04"] -> Day04.runSolution "input/Day04"
	args -> error $ "Args were: " ++ show args
