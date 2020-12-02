module Main where

import qualified Day02
import qualified System.Environment as SE

main :: IO ()
main = SE.getArgs >>= Day02.runSolution . (!! 0)
