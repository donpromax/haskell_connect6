module AI.Path (board_scores) where
  

import Data.List (transpose)
import Utils (allSubs,diagonals,diagonals')


----------------------
-------- Data --------
----------------------
type Board = [[Int]]
                                   

----------------------
------ Funcions ------
----------------------
{-
  Score = sum(numOfBlackRoad[i]*Score[i]) - sum(numOfWhiteRoad[i]*Score[i])
-}
board_scores :: Board -> Int
board_scores [] = 0
board_scores board =
  let row = score_rows board
      col = score_cols board
      diag = score_diags board
      diag' = score_diags' board
  in  row + col + diag + diag' 


----------------------
----- Help-Func ------
----------------------
-- 根据棋盘的所有行计算score
score_rows :: Board -> Int
score_rows [] = 0
score_rows (x:xs) = 
  let possible_paths = allSubs x 6
  in sum (map score_path possible_paths) + score_rows xs
  
score_cols = score_rows . transpose

score_diags = score_rows . diagonals

score_diags' = score_rows . diagonals'
  
  
{-
  输入一个长度为6的[Int] List
  如果是合法的路，返回score。否则返回0
  注意，黑棋得分为正，白棋得分为负
-}
score_path :: [Int] -> Int
score_path [] = 0 
score_path path = 
  let black = score_path_black path 
      white = score_path_white path
      freq = black + white
  in  case freq of 
      0 -> 0
      1 -> 20
      2 -> 80
      3 -> 150
      4 -> 800
      5 -> 1000
      6 -> 100000
      (-1) -> (-20)
      (-2) -> (-80)
      (-3) -> (-150)
      (-4) -> (-800)
      (-5) -> (-1000)
      (-6) -> (-100000)
  where 
    score_path_black :: [Int] -> Int
    score_path_black path = if all (\y -> y == 0 || y == 1) path
                            then sum path
                            else 0
                                    
    score_path_white :: [Int] -> Int
    score_path_white path = if all (\y -> y == 0 || y == 2) path
                            then div (sum path) (-2)
                            else 0