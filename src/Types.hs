module Types where

data FileStats = FileStats
  { filePath          :: FilePath
  , totalLines        :: Int
  , commentLines      :: Int
  , todoLines         :: Int
  , functionCount     :: Int
  , averageFuncLength :: Double
  , fileSize          :: Integer
  }
