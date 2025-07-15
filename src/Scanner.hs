module Scanner (getSourceFiles) where

import System.Directory
import System.FilePath
import Control.Monad (filterM)

getSourceFiles :: FilePath -> IO [FilePath]
getSourceFiles path = do
    contents <- listDirectory path
    let fullPaths = map (path </>) contents
    files <- filterM doesFileExist fullPaths
    dirs  <- filterM doesDirectoryExist fullPaths
    nested <- fmap concat (mapM getSourceFiles dirs)
    return $ filter isCodeFile files ++ nested

isCodeFile :: FilePath -> Bool
isCodeFile f = takeExtension f `elem`
    [ ".hs"
    , ".py"
    , ".js"
    , ".ts"
    , ".cpp"
    , ".c"
    , ".java"
    , ".go"
    , ".rb"
    , ".php"
    , ".rs"
    , ".csv"
    , ".html"
    , ".pl"    -- Prolog
    , ".zig"
    ]
