module Records.CsvWriter (genBlackCsv
                         ,genWhiteCsv) where
  

import System.Directory (removeFile, renameFile)
import System.IO (openTempFile,hClose,hPutStr)
import Control.Exception

import Records.GameRecord (record2line)

----------------------
-------- Data --------
----------------------
type FileName = String
type Records = [[String]]
type SingleRecord = [String]

----------------------
------ Funcions ------
---------------------- 
{- 
  用游戏记录List，写black.csv
-}
genBlackCsv :: Records -> IO ()
genBlackCsv = genCsvFile "black.csv"

{- 
  用游戏记录List，写white.csv
-}
genWhiteCsv :: Records -> IO ()
genWhiteCsv = genCsvFile "white.csv"
        


----------------------
----- Help-Func ------
----------------------

genCsvFile :: FileName -> Records -> IO ()
genCsvFile fileName records  = do
  let content = unlines $ map record2line records
  
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      hPutStr tempHandle content
      hClose  tempHandle
      try (removeFile fileName) :: IO (Either SomeException ())
      renameFile tempName fileName)
      
