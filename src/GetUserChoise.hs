module GetUserChoise
       ( printHelp
       , parseArguments
       ) where

import Lib
import System.Exit
import System.IO
import Text.Printf

printHelp :: String -> IO()
printHelp x = do
     putStrLn ("USAGE:" ++ "\t\t" ++ "./" ++ x ++ " math-expression")
     putStrLn("\t\t" ++ "write a valid mathematical expression and wait for the result!")
     putStrLn("Exemple:" ++ "\t" ++ "3 + 2 * (1 - 3)")

createList :: Int -> [Int]
createList nbr =
  if (nbr == 0)
    then []
  else
    0 : createList (nbr - 1)

parseArguments :: [String] -> IO ()
parseArguments args = do
  let arg1 = args !! 0
  case (evaluate . buildTree . getTokens) (arg1) of
      Right result -> (printf "%.2f\n" . roundResult)(result)
      Left err -> do
          printf "%s\n" err
          exitWith (ExitFailure 84)
