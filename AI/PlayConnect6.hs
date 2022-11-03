{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module AI.PlayConnect6 (auto,manual,restart,showBoard) where

import Control.Exception
import Data.Time.Clock
import Text.Read (readEither)
import System.Directory (removeFile)

import Records.Times (plusSeconds)
import Records.GameRecord (readcontent,reformatRecords,getRecord)
import AI.MovesGenerator
import AI.Checkmate (win)
import Boards.Checkerboard (printBoard
                           ,updateBoard)

import Utils (change_elem)
import IOUtils (logError)
import Records.CsvWriter (genBlackCsv
                         ,genWhiteCsv)
import System.IO

----------------------
-------- Data --------
----------------------
type Records = [[String]]
type SingleRecord = [String]
type Board = [[Int]]
type Solution = [(Int,Int)]

empty_board = replicate 15 (replicate 15 0)
----------------------
------ Funcions ------
----------------------
restart :: IO ()
restart = do
  try (removeFile "white.csv") :: IO (Either SomeException ())
  try (removeFile "black.csv") :: IO (Either SomeException ())
  return ()

manual :: Int -> IO ()
manual i = do
  (black_records,white_records) <- readRecords
  solution <- userInput (black_records,white_records)
  case solution of
    Left _  -> return ()
    Right s -> play (black_records,white_records) (\_ -> s) i


{-
  调用AI自动下棋
-}
auto :: Int -> IO ()
auto 0 = do
  (black_records,white_records) <- readRecords
  play (black_records,white_records) black_ai 0

auto 1 = do
  (black_records,white_records) <- readRecords
  play (black_records,white_records) white_ai 1

auto _ = logError "请输入:\n0,运行黑棋;\n1,运行白棋!"



showBoard :: IO ()
showBoard = do
  (black_records,white_records) <- readRecords
  let board' = updateBoard empty_board 1 black_records
      board = updateBoard board' 2 white_records

  printBoard board


----------------------
----- Help-Func ------
----------------------
play :: (Records,Records) -> (Board -> Solution) -> Int -> IO ()
play (black_records,white_records) f i = if i /= 0 && i /= 1 then logError "请输入:\n0,运行黑棋;\n1,运行白棋!" else do
  let valr = valRecords i (black_records,white_records)
  if (valr /= "") then logError valr else do
    beforetime <- getSeconds
    let board' = updateBoard empty_board 1 black_records
        board = updateBoard board' 2 white_records
        solution = f board
        vals = valSolution solution board
    if (not . null) vals then logError vals else do
      aftertime <- getSeconds
      let cost = floor (aftertime - beforetime)
          (black_records',white_records') = mergeRecords cost i solution (black_records,white_records)
          newBoard = applySolution board i solution

      genBlackCsv black_records'
      genWhiteCsv white_records'
      printBoard newBoard
      putStrLn $ showSolution i solution


applySolution :: Board -> Int -> Solution -> Board
applySolution board i s =
      let n = if i == 0 then 1 else 2
          board' = change_elem board (head s) n
      in change_elem board' (last s) n


userInput :: (Records,Records) -> IO (Either () Solution)
userInput (black_records,white_records) = do
  row1 <- getNum "请输入第一个落子点的行号"
  let numError = "输入的坐标必须是介于1到15的整数"
  if not (verify row1)
  then do
    logError numError
    return $ Left ()
  else do
    col1 <- getNum "请输入第一个落子点的列号"
    if not (verify col1)
    then do
    logError numError
    return $ Left ()
    else if length black_records == 0 then return $ Right [(row1-1,col1-1)] else do
      row2 <- getNum "请输入第二个落子点的行号"
      if not (verify row2)
      then do
      logError numError
      return $ Left ()
      else do
        col2 <- getNum "请输入第二个落子点的列号"
        if not (verify col2)
        then do
        logError numError
        return $ Left ()
        else return $ Right [(row1-1,col1-1),(row2-1,col2-1)]
  where
    getNum msg = do
      putStr msg
      numStr <- getLine
      let num = case readEither numStr :: Either String Int of
                  Left l -> -1
                  Right r -> r
      return num
    verify num = num > 0 && num <= 15


-- 输入计算时间、0黑/1白、解、黑白records，返回新增后的黑白records
mergeRecords seconds i solution (black_records,white_records) =
  let n = if i == 0
          then 1 + length black_records
          else 1 + length white_records
      enemyTime = getEnemyTime i (black_records,white_records)
      ourTime = plusSeconds enemyTime seconds
      newRecord = getRecord ourTime n solution
      black_records' = reformatRecords black_records
      white_records' = reformatRecords white_records in
  if i == 0
  then (black_records' ++ [newRecord],white_records')
  else (black_records',white_records' ++ [newRecord])

{-
  根据black.csv white.csv读取记录，转换为Records([[String]])格式
-}
readRecords :: IO (Records,Records)
readRecords = do
  try_white <- try (readFile "white.csv") :: IO (Either SomeException String)
  try_black <- try (readFile "black.csv") :: IO (Either SomeException String)
  let white_content = case try_white of
        Left  l -> ""
        Right r  -> r
      black_content = case try_black of
        Left  l -> ""
        Right r  -> r
  return (readcontent black_content,readcontent white_content)

-- 获取当前系统时间秒数
getSeconds = getCurrentTime >>= return .
                                fromRational .
                                toRational .
                                utctDayTime

-- 获取敌方最后一手的时间"hh/mm/ss"
getEnemyTime :: Int -> (Records,Records) -> String
getEnemyTime i (black_records,white_records) =
  if i == 0
  then head $ getLast white_records
  else head $ getLast black_records
  where
    getLast records = if length records == 0
                      then ["00/00/00"]
                      else last records

-- 检查Records是否有违规
valRecords :: Int -> (Records,Records) -> String
valRecords i (black_records,white_records)
  | i == 0 && length black_records /= length white_records = "请等待白方落子后再掷子"
  | i == 1 && length black_records /= 1 + length white_records = "请等待黑方落子后再掷子"
  | otherwise = ""

valSolution :: Solution -> Board -> String
valSolution solution board =
  let (x1,y1) = head solution
      (x2,y2) = last solution in
   if (not . null) $ win board
   then "已有获胜方!"
   else
     if board!!x1!!y1 /= 0 || board!!x2!!y2 /= 0
     then "禁止在已经有子的位置落子!"
     else ""

-- Solution转为字符串显示
showSolution i solution =
  let solution' = map (\(x,y) -> (x+1,y+1)) solution
  in (if i == 0 then "黑方" else "白方") ++
     "得到解：" ++ show solution' ++ "\n"





    