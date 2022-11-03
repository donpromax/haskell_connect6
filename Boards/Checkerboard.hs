module Boards.Checkerboard (printBoard
                           ,updateBoard) where
  

--import System.IO ()

import Boards.Graphic
import Utils 

----------------------
-------- Data --------
----------------------
 
blank = ["     #"
        ,"     #"
        ,"######"]
        
black = ["######"
        ,"######"
        ,"######"]
        
white = [".....#"
        ,".....#"
        ,"######"]
        
        
type Board = [[Int]]
type Records = [[String]]
type SingleRecord = [String]
                               
----------------------
------ Funcions ------
----------------------
updateBoard :: Board -> Int -> Records -> Board
updateBoard oldboard _ [] = oldboard
updateBoard oldboard player (r:rs) =
  let move1 = read (r!!2) :: (Int,Int)
      move2 = read (r!!3) :: (Int,Int)
      board_1 = change_elem oldboard move1 player
      board_2 = change_elem board_1 move2 player
      board = if length r == 3
              then board_1
              else board_2
  in  updateBoard board player rs

{-|
  函数'printBoard'打印棋盘的字符串列表形式
  @input    棋盘
  @output   打印Picture的IO
-}
printBoard :: Board -> IO ()
printBoard = printPicture . getBoard


----------------------
----- Help-Func ------
----------------------
{-|
  函数'getBoard'获取棋盘的字符串列表形式
  @input    棋盘
  @output   字符串列表的Picture格式
-}
getBoard :: Board -> Picture
getBoard =  padding . board2pic

-- 把[[Int]]格式转换成可打印的Picture格式（无padding）
board2pic :: Board -> Picture
board2pic [] = []
board2pic (x:xs) = above (printLine x) (board2pic xs)

-- 打印一行
printLine :: [Int] -> Picture
printLine [] = []
printLine (x:xs) 
  | x == 0  = sideByside blank (printLine xs)
  | x == 1  = sideByside black (printLine xs)
  | x == 2  = sideByside white (printLine xs)
  

-- 补齐最左边以及第一行的棋盘边界
padding :: Picture -> Picture
padding [] = []
padding board = margin:board' ++ [col_num]
  where
    board' = [show_row_num (fst line) ++ '#':snd line | 
              line <- zip [0..] board]
              
    margin = last board'
    col_num = [show_col_num n|n <- [0..len-1]]
    len = length $ margin
    
    block_width  = length $ head blank
    block_height = length blank
    show_row_num n = 
      let m = n `mod` block_height
          q = 1 + n `div` block_height
      in if m == 0 
         then if q > 9 then (show q) else ("0" ++ show q)
         else "  "
    
    show_col_num n =
      let m'   = (n-4) `mod` block_width
          m    = (n-5) `mod` block_width
          q    = 1 + (n-4) `div` block_width
          q_1  = q `mod` 10
          q_10 = q `div` 10
      in if m' == 0     then head $ show q_10
         else if m == 0 then head $ show q_1
         else ' '

        
         
    
    




