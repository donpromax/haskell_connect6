module AI.Judge (runJudge) where


import AI.Checkmate (win)
import Boards.Checkerboard (printBoard)
import Data.List
import Records.GameRecord (readblack,readwhite,record2player)
import Records.Times (timeover)
import Utils (change_elem)
import IOUtils (printInfo)

----------------------
-------- Data --------
----------------------
type Records = [[String]]
type SingleRecord = [String]
type Board = [[Int]]

----------------------
------ Funcions ------
----------------------
runJudge = do
  content_b <- readFile "black.csv"
  content_w <- readFile "white.csv"
  let board = replicate 15 (replicate 15 0)
      black = readblack content_b
      white = readwhite content_w
      comb  = (black ++ white)
      records = sortOn (\xs -> xs!!1) comb
  if whiteFirst records 
  then printInfo "违规:白方违规先手" board []
  else firstRound records board
  



----------------------
----- Help-Func ------
----------------------
{-|
  函数'occupied'判断当前位置是否有棋子占用
  @input    棋盘，坐标
  @output   True有占用，否则False
-}
occupied :: Board -> (Int,Int) -> Bool
occupied board (row,col) = board!!row!!col /= 0 

{- 
  判断是否白方违规先手 
-}
whiteFirst :: Records -> Bool
whiteFirst r = (head r) !! 1 !! 0 == 'w'

{- 
  输入为单个操作: 同为黑子or同为白子返回True，否则返回False 
-}
samePlayer :: SingleRecord -> SingleRecord -> Bool
samePlayer r1 r2 = last (r1!!1) == last (r2!!1)

-- 第一回合
firstRound :: Records -> Board -> IO ()
firstRound records board = do
  let entry = head records
  if length entry /= 3 
  then printInfo "违规:第一手只能落一子" board entry
  else do
    let (x,y) = read (entry !! 2) :: (Int,Int) 
        newBoard = change_elem board (x,y) 1
    newRound entry (tail records) newBoard
  

-- 新的回合:上一回合的记录->剩余全部记录->当前棋盘
newRound :: SingleRecord -> Records -> Board -> IO ()
newRound sr [] board = printInfo "目前暂未有获胜方" board sr
newRound last_entry records board = do
  let entry = head records -- 本回合的记录
      player = record2player entry -- 1:black 2:white
      playerStr = if player == 1 then "黑方" else "白方"
      (x1,y1) = read (entry !! 2) :: (Int,Int) -- 坐标1
      (x2,y2) = read (entry !! 3) :: (Int,Int) -- 坐标2
      newBoard' = change_elem board (x1,y1) player 
      newBoard  = change_elem newBoard' (x2,y2) player -- 落子后的棋盘
  if samePlayer entry last_entry -- 此手与上一手是同一个玩家
  then printInfo ("违规:" ++ playerStr ++ "试图连续掷子!") board entry
  else 
    if timeover (head entry) (head last_entry) -- 超时(>5s)
    then printInfo "违规:思考时间大于5s" board entry
    else -- 位置上已有棋子?
      if (x1,y1) == (x2,y2) || occupied board (x1,y1) || occupied board (x2,y2) 
      then printInfo "违规:不能在已有棋子的位置上落子" board entry
      else do-- 落子
        let winner = win newBoard
        if  winner == [] -- 判断是否有胜利方
        then do
          printInfo "" board last_entry
          putStrLn "\n"
          newRound entry (tail records) newBoard --无获胜方 下一回合
        else 
          if length records /= 1 -- 已有winner，后续是否还有记录
          then printInfo ("违规:已有获胜方(" ++ playerStr ++ "),不能继续落子") newBoard (records!!1)
          else do
            printInfo "" board last_entry
            putStrLn "\n"
            printInfo ("已有获胜方:" ++ playerStr ++ '\n':showWinner winner) newBoard entry       
  where showWinner x = "获胜坐标为" ++ x!!0 ++ " ,方向为:" ++ x!!1  