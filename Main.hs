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

-- The defaults for the parameters
defaultOptions :: Options
defaultOptions = Options { optOption   = Nothing
                         , optArgument = Nothing
                         }
{-
 - Describes the valid options for the app. An array of 
 - OptDescr a, where 'a' is (Option -> IO Option).
 -
 - Each element of the array has four components that describe a single option:
 -
 - The arguments to Option (the OptDescr constructor) are:
 -
 -  * list of short option characters
 -  * list of long option strings (without "--")
 -  * argument descriptor
 -  * explanation of option for user
 -
 - Option [Char] [String] (ArgDescr a) String	 
 -
 - The ArgDescr (argument descriptor) is interesting in this case.
 - Note that the ReqArg and OptArg constructors take functions as
 - arguments: (String -> a) and (Maybe String -> a).
 - 
 - Here is the ArgDescr date type:
 -
      data ArgDescr a
        = NoArg                   a         -- no argument expected
        | ReqArg (String       -> a) String -- option requires argument
        | OptArg (Maybe String -> a) String -- optional argument
 -
 - For example, look at the ReqArg constructor of ArgDescr:
 -
        ReqArg (String -> a) String
 -
 - Recall that in this example 'a' is of type (Options -> IO Options).
 -
 - Substituting (Options -> IO Options) for 'a' in (String -> a) 
 - we get ...
 -
    String -> (Options -> IO Options ) which is just
    String -> Options -> IO Option
 -
 - Now the types of the lambdas in the List of OptDescr below 
 - should make sense.
 -}
optionDescriptions :: [ OptDescr ( Options ->IO Options ) ]
optionDescriptions =
  [ Option "o" [ "option" ]
      ( OptArg ( \arg opt -> return $ opt { optOption = arg } ) "" )
      "Set this optional command-line option"

  , Option "a" [ "argument" ]
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
    let (actions, nonOptions, errors) = getOpt Permute optionDescriptions args
 
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return defaultOptions) actions

    printArgs opts

    mapM_ putStrLn nonOptions
    putStrLn "Good-bye!"

 




