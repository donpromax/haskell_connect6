{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module AI.MovesGenerator (black_ai,white_ai,black_generator) where
  
import Data.List
import Data.Maybe (fromMaybe)

import AI.Checkmate (checkmate_b,checkmate_w,win)
import AI.Path (board_scores)
import Utils
import Boards.Checkerboard

----------------------
-------- Data --------
----------------------
type Board = [[Int]]
type Solution = [(Int,Int)]                                
convert b = [[if x == 1 then 2 else if x == 2 then 1 else 0 |x <- rows]|rows <- b] 
----------------------
------ Funcions ------
----------------------
white_ai :: Board -> Solution
white_ai = black_ai . convert
    
black_ai :: Board -> Solution
black_ai board = if null solutions then [] else
  (snd . maximum) scores
  where
    solutions = black_generator board 
    scores = map (cal_scores board 1) solutions

{-
  黑子的走法生成器
-}
black_generator :: Board -> [Solution]
black_generator board = if not $ null (win board) then [] else
  if not . null $ black_checkmate
  then complete_sol (head black_checkmate) board else
    if not . null $ white_checkmate
    -- 白棋正在四子 五子连珠
    then defense
    -- 一般情况，先检查是否可以形成“双四连珠”等杀棋
    else all_valid_moves
--      if not . null $ kill_move_b
--      then [head kill_move_b]
--      else all_valid_moves
  where
    black_checkmate = checkmate_b board
    white_checkmate = checkmate_w board
    valid_points = getPos board
    all_valid_moves = if length valid_points == 1 then [valid_points] else choose 2 valid_points
    --
    -- 需要防守时，棋盘上有 威胁点位和普通点位
    threat_points = rmdups $ concat white_checkmate

    -- 防守分为完全防守和部分防守: 
    -- 取杀棋落子点自我结合; 杀棋落子点和普通位置的两两组合 
    defense' =  filter (null . checkmate_w . applySolution board 1) . rmdups $ 
                choose 2 threat_points ++ [[x,y] | x <- threat_points, y <- valid_points]
    defense = if null defense' then [head all_valid_moves] else defense'
    
    kill_move_b = filter (isKillMove_b board) all_valid_moves
    
----------------------
----- Help-Func ------
----------------------
{-
  给定一个解，确定能不能产生“四四连珠”等杀棋的情况
  True: 该解可以产生绝杀
  否则返回False
-}
isKillMove_b :: Board -> Solution -> Bool
isKillMove_b board solution = 
  not (null checkmates') && 
  all (not . null) checkmates
  where 
    board' = applySolution board 1 solution
    -- 黑棋落子后，产生的杀棋点
    checkmates' = checkmate_b board'
    -- 白棋无论怎么尝试去阻挡，黑棋都仍然有杀棋点存在
    checkmates = map (checkmate_b . applySolution board' 2) .
                 choose 2 . rmdups . concat $ checkmates'


{-
  将一个Solution应用到棋盘上[()] or [(),()]
-}
applySolution :: Board -> Int -> Solution -> Board
applySolution board _ [] = board
applySolution board i s = 
      let board' = change_elem board (head s) i
      in change_elem board' (last s) i
     
{-
  根据现有棋盘，解Solution，黑子还是白子。计算落子后的(score,solution)
-}
cal_scores :: Board -> Int -> Solution -> (Int, Solution)
cal_scores board i s = (board_scores $ applySolution board i s,s)


{-如果Solution长度只有一，与其他合法位置结合-}
complete_sol :: Solution -> Board -> [Solution]
complete_sol s board = if length s == 2 then [s]
                       else filter (not . null) $ 
                            map (\x -> if x /= head s then x:s else []) (getPos board)

{-
  合法位置:两颗子范围内有其他子
-}
getPos :: Board -> [(Int,Int)]
getPos [] = []
getPos board = 
  if board == empty_board then [(div n 2, div n 2)] else
    filter (validPos board) unoccupied
  where
    n = length board
    empty_board = replicate n (replicate n 0)
    -- 所有可落子的范围
    possible_pos = [(x,y)|x<-[0..n-1],y<-[0..n-1]]
    unoccupied = filter (\(x,y) -> board!!x!!y == 0) possible_pos
    
    
---- 上下左右函数，取已落子外接矩形，外延2
--top = top' . zip [0..]
--  where
--    top' (x:xs) = 
--      if all (== 0) (snd x) 
--      then top' xs else
--        if (fst x) - 2 >= 0 then (fst x) - 2
--        else if (fst x) - 1 >= 0 then (fst x) - 1
--        else fst x
--bottom board = length board - 1 - (top . reverse $ board)
--left = top . transpose
--right board = length board - 1 - (left . map reverse $ board)

-- 该子在水平 垂直 左斜 右斜方向 距离2以内有子
validPos :: Board -> (Int,Int) -> Bool
validPos board (x,y) =
  let a = [(x-1,y),(x-2,y),(x+1,y),(x+2,y)] ++
          [(x,y-1),(x,y-2),(x,y+1),(x,y+2)] ++
          [(x-1,y-1),(x-2,y-2),(x+1,y+1),(x+2,y+2)] ++
          [(x+1,y-1),(x+2,y-2),(x-1,y+1),(x-2,y+2)]
      b = map (\(x,y) -> inRange x y && board!!x!!y /= 0) a       
  in or b
  where
    n = length board
    inRange x y = 0 <= x && x < n && 0 <= y && y < n
      


  