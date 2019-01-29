module Main where

import System.Environment
import System.Exit

import Lib
import GetUserChoise

main :: IO()
main = do parse <- getArgs
          nameProg <- getProgName
          case parse of
            [] -> do
              printHelp nameProg
              exitWith (ExitFailure 84)
            otherwise -> parseArguments parse
