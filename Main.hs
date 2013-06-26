module Main where

import System.Console.GetOpt
import System.Environment
import System.Exit
import Control.Monad
import System.IO
import Data.Char
import Data.List

version :: String
version = "0.1.0"

{- These optional arguments could easily be Strings.  GetOpt handles their
 - optional nature just fine.  I like having them as Maybe values though.
 -}
data Options = Options { optOption      ::Maybe String
                       , optArgument    ::Maybe String
                       }

defaultOptions :: Options
defaultOptions = Options { optOption   = Nothing
                         , optArgument = Nothing
                         }
{-
ReqArg
  (String -> a)  -- Usual ArgDescr for Required arg. (The higher order function bit anyway)

When 'a' is Option -> IO Option then, substituting for 'a', we get:

String -> Option -> IO Option

Voila!!
-}

optionDescriptions :: [ OptDescr ( Options ->IO Options ) ]
optionDescriptions =
  [ Option "o" [ "option" ]
      ( OptArg ( \arg opt -> return $ opt { optOption = arg } ) "" )
      "Set this optional command-line option"

  , Option "c" [ "carrier" ]
      ( OptArg ( \arg opt -> return opt { optArgument = arg } ) "" )
      "Give this hypothetical command-line argument"

  , Option "v" [ "version" ]
      ( NoArg ( \_ -> ( putStrLn $ "This is haskell arg processor v" ++ version ) >>  exitWith ExitSuccess ) )
      "Print version information"

  , Option "h" [ "help" ]
      ( NoArg ( \_ -> ( putStrLn $ usageInfo "argproc" optionDescriptions ) >>  exitWith ExitSuccess ) )
      "Print this usage information"
  ]

printArgs :: Options -> IO ()
printArgs opts =
  do
    case optOption opts of
      Just s -> putStrLn $ "Option: " ++ s
      _      -> putStrLn $ "Option not given"
    case optArgument opts of
      Just s -> putStrLn $ "Argument: " ++ s
      _      -> putStrLn $ "Argument not given"

main = 
  do
    args <- getArgs
 
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder optionDescriptions args
 
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return defaultOptions) actions

    printArgs opts

    mapM_ putStrLn nonOptions
    putStrLn "Good-bye!"

 




