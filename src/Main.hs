{-|
Module      : Main
Description : Implementation of a lexical analysis tool for the 
              Rockstar programming language.
Copyright   : (c) Andrés Ignacio Torres, 2018
License     : MIT
Maintainer  : andresitorresm+github@gmail.com
Stability   : stable

Implements a lexical analysis tool for the Rockstar programming
language, using an Alex generated grammar. This client reads an
specified file and tries to analyse its content and print
the tokens found to the standard output.

Note: You can find the current specification of the Rockstar 
      programming language, made by Dylan Beattie at the following 
      link:     
            https://github.com/dylanbeattie/rockstar
-}
module Main (main) where

import System.Environment
import System.Exit
import qualified Lexer as L

-- | The 'showArgumentError' function handles a missing argument error and quits.
showArgumentError :: IO ()
showArgumentError = do
    name <- getProgName
    putStrLn ("Fatal error: missing file argument.")
    putStrLn("\tUsage: " ++ name ++ "<filename>")
    exitWith (ExitFailure 1)

-- | The 'showHelpText' function shows a help text and quits.
showHelpText :: IO ()
showHelpText = do
    name <- getProgName
    putStrLn (name ++ ": a lexical analysis tool for the Rockstar programming language.")
    putStrLn ("\tUsage: " ++ name ++ " <filepath>            Analyses a file located  in <filepath>.")
    putStrLn ("\t       " ++ name ++ " --help                Show this help text.")
    exitWith ExitSuccess

-- | The 'scanFile' function scans a file for tokens and prints them on screen, 
--   then quits.
scanFile :: FilePath -> IO ()
scanFile file = do
    content <- readFile file
    let tokens = L.alexScanTokens content
    L.printScannedTokens tokens
    exitWith ExitSuccess

-- | The 'main' procedure tries to read a file passed as a first argument, 
--   show the tokens in the file, and quits.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> showArgumentError
        firstArg:_ -> do
            if elem firstArg ["--help", "-h"] 
                then showHelpText
            else scanFile firstArg