module Main where

import System.Environment (getArgs)
import System.Directory (doesDirectoryExist)
import Scanner (getSourceFiles)
import Analyzer (analyzeFiles, printReport)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [folder] -> do
            exists <- doesDirectoryExist folder
            if exists then do
                files <- getSourceFiles folder
                stats <- analyzeFiles files
                printReport stats
            else
                putStrLn "❌ Provided path is not a directory."
        _ -> putStrLn "Usage: statix <directory>"
