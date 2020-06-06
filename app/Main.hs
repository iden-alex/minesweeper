module Main where

import System.Random
import GameAction

main :: IO ()
main = do
    gen <- getStdGen
    startGame gen
