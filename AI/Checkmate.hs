{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module AI.Checkmate (win
                    ,checkmate_b
                    ,checkmate_w
                    ,Solution) where
  
import Data.List
import Utils (search_board, indexOf)

----------------------
-------- Data --------
----------------------
type Pattern = [Int]
type Board = [[Int]]
type Solution = [(Int,Int)]
type SearchResult = [String]

black_paths = [[a1,a2,a3,a4,a5,a6]|a1 <- [1,0],
                                   a2 <- [1,0],
                                   a3 <- [1,0],
                                   a4 <- [1,0],
                                   a5 <- [1,0],
                                   a6 <- [1,0]]

white_paths = [[a1,a2,a3,a4,a5,a6]|a1 <- [2,0],
                                   a2 <- [2,0],
                                   a3 <- [2,0],
                                   a4 <- [2,0],
                                   a5 <- [2,0],
                                   a6 <- [2,0]]
                               
-- 五子”路“
black5 :: [[Int]]
black5 = [x | x <- black_paths, sum x == 5]

-- 四子”路“  
black4 :: [[Int]]     
black4 = [x | x <- black_paths, sum x == 5]
         
black_offense = black5 ++ black4
         
white_offense = [[if x == 1 then 2 else x|x <- y]
                                         |y <- black_offense]



----------------------
------ Funcions ------
----------------------


{-|
  函数'win'寻找是否存在六子相连的情形
  @input    棋盘
  @output   按行、列、主对角线、次对角线次序
            判断是否有winner(6子一线)。
            黑子胜利: [起始点坐标,方向]
            白子胜利: [起始点坐标,方向]
            无winner:[]
            (该函数只要找到一个解就会停止)
-}
win :: Board -> [String]
win board = let black_win_pat = replicate 6 1
                white_win_pat = replicate 6 2
                black = search_board board black_win_pat
                white = search_board board white_win_pat
            in  if not $ null black
                then head black
                else if not $ null white
                then head white
                else []

{- 返回所有的黑子杀棋落子位置 -}
checkmate_b :: Board -> [Solution]
checkmate_b board = checkmate board black_offense


{- 返回所有的白子杀棋落子位置 -}
checkmate_w :: Board -> [Solution]
checkmate_w board = checkmate board white_offense


----------------------
----- Help-Func ------
----------------------
    
-- 在棋盘中依次搜索模式列表，返回杀棋落子位置
checkmate :: Board -> [Pattern] -> [Solution]
checkmate board [] = []
checkmate board (p:ps) = 
  let results = search_board board p
  in if not $ null results
     then map (handle_result p) results ++ checkmate board ps
     else checkmate board ps

{-
  根据search_board的结果集和搜索pattern决定落子位置
-}
handle_result :: Pattern -> SearchResult -> Solution
handle_result [] _ = []
handle_result pat result = 
  let (i,j) = read (head result) :: (Int,Int)
      indices = indexOf pat 0
      direction = last result
  in  case direction of
        "row" -> map (\x -> (i,j+x)) indices
        "col" -> map (\x -> (i+x,j)) indices
        "diag" -> map (\x -> (i-x,j+x)) indices
        "diag'" -> map (\x -> (i-x,j-x)) indices
        
