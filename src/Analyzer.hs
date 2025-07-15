module Analyzer (analyzeFiles, printReport) where

import Types
import System.IO
import Data.List (isPrefixOf, isInfixOf)
import System.Directory (getFileSize)

analyzeFiles :: [FilePath] -> IO [FileStats]
analyzeFiles = mapM analyzeFile

analyzeFile :: FilePath -> IO FileStats
analyzeFile path = do
    content <- readFile path
    size <- getFileSize path
    let ls = lines content
        total = length ls
        comments = length $ filter isComment ls
        todos = length $ filter (\l -> "TODO" `isInfixOf` l) ls
        funcs = filter isFuncStart ls
        funcCount = length funcs
        avgFuncLen = if funcCount == 0 then 0 else fromIntegral total / fromIntegral funcCount
    return $ FileStats path total comments todos funcCount avgFuncLen size

isComment :: String -> Bool
isComment l = any (`isPrefixOf` dropWhile (== ' ') l) ["#", "//", "--"]

isFuncStart :: String -> Bool
isFuncStart l = any (`isPrefixOf` dropWhile (== ' ') l) ["def ", "function ", "fn ", "void ", "int ", "float ", "double "]

printReport :: [FileStats] -> IO ()
printReport stats = do
    mapM_ printStat stats
    putStrLn $ "Total files analyzed: " ++ show (length stats)

printStat :: FileStats -> IO ()
printStat fs = do
    putStrLn $ "File: " ++ filePath fs
    putStrLn $ "Lines: " ++ show (totalLines fs)
             ++ " | Comments: " ++ show (commentLines fs)
             ++ " | TODOs: " ++ show (todoLines fs)
             ++ " | Functions: " ++ show (functionCount fs)
             ++ " | Avg Func Len: " ++ show (averageFuncLength fs)
    putStrLn $ "File size: " ++ show (fileSize fs) ++ " bytes"
    putStrLn ""
