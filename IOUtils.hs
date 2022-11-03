module IOUtils (printInfo
               ,logError) where
  
import Boards.Checkerboard (printBoard)
import Records.GameRecord (showRecord)
import Data.Time



type Board = [[Int]]
type Records = [[String]]
type SingleRecord = [String]
----------------------
------ Funcions ------
----------------------
{-
  打印信息，并打印棋盘
-}
printInfo :: String -> Board -> SingleRecord -> IO ()
printInfo msg board record = do
  printBoard board
  putStrLn msg
  putStrLn $ "最后一项尝试的操作: " ++ showRecord record
  
    
logError :: String -> IO ()
logError msg = do
  zonedTime <- fmap show getZonedTime
  let content = '[':zonedTime ++ "] " ++ msg ++ "\n"
  appendFile "log.txt" content
  putStrLn msg
----------------------
----- Help-Func ------
----------------------

                   
                   
