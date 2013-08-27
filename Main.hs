module Main where

import System.Console.GetOpt
import System.Environment

import qualified Options as O


printIndentedListWithHeading :: String -> [ String ] -> IO ()
printIndentedListWithHeading _ [] = return ()
printIndentedListWithHeading h xs =
  do
    putStrLn ""
    putStrLn h
    -- Map over the strings, printing them out.
    mapM_ putStrLn $ zipWith (++) (map ( (\s -> "  " ++ s ++ " ") . show ) [1 :: Int ..] ) xs
    putStrLn ""


main :: IO ()
main = 
  do
    args <- getArgs
 
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt Permute O.optionDescriptions args
 
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return O.defaultOptions) actions

    O.printArgs opts

    -- Print out the non-option arguments.
    printIndentedListWithHeading "Non-option arguments" nonOptions

    -- Print out the errors.
    printIndentedListWithHeading "Errors" errors
 
    putStrLn "Good-bye!"

 




