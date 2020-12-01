module Main where

import qualified Day01
import qualified System.Environment as SE

main :: IO ()
main = SE.getArgs >>= Day01.runSolution . (!! 0)
