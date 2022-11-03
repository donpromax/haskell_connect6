{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Utils (replaceNth
             ,change_elem
             ,find'
             ,countOfPat
             ,search_board
             ,diagonals
             ,diagonals'
             ,indexOf
             ,allSubs
             ,choose
             ,rmdups) where

import Data.List
import Data.Maybe
import Control.Monad.State


----------------------
------ Funcions ------
----------------------
-- 去重List
-- >>> rmdups [1,2,2,3]
-- >>> group [1,2,2,3]
-- [1,2,3]
-- [[1],[2,2],[3]]
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

{-
  返回列表的所有组合
>>> choose 2 [1,2,3,4]
[[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
-}
choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ []= []
choose n (x:xs) = map (x :) (choose (n-1) xs) ++ choose n xs

{- 
  return 长度为n的所有子串
>>> allSubs [1,2,3,4] 2
[[1,2],[2,3],[3,4]]
-}
allSubs :: [a] -> Int -> [[a]]
allSubs s n
    | length s >= n = take n s : allSubs (tail s) n
    | otherwise = []

{-|
  函数'indexOf'获取元素的索引
  @input     List和要查找的元素
  @output    索引值列表
  indexOf [0,1,1,1,1,0] 0 = [0,5]
-}
indexOf :: Eq a => [a] -> a -> [Int]
indexOf xs x = filter ((== x) . (xs !!)) [0..length xs - 1]

-- >>> ([1,2,3]:[])
-- [[1,2,3]]
{-|
  函数'diagonals'获取矩阵的所有对角线(左下到右上)
  @input     二维方针
  @output    对角线组成的新的二维矩阵
>>> diagonals [[1,2],[3,4]]
[[1],[3,2],[4]]
-}
diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
  go b m = let ts = map tail b in
    map head b : case m of
         []   -> transpose ts
         x:xs -> go (x:ts) xs

{-|
  函数diagonals'获取矩阵的所有对角线(右下到左上)
  @input     二维方针
  @output    对角线组成的新的二维矩阵
>>> diagonals' [[1,2],[3,4]]
[[2],[4,1],[3]]
-}
diagonals' :: [[a]] -> [[a]]
diagonals' = diagonals . map reverse

{-|
  acts like python 'find'. 
  returns the leftmost element index
  or -1 if there is no such element.
>>> find' "bc" "abcbcd"
1
-}
find' :: (Eq a) => [a] -> [a] -> Int
find' pat str = fromMaybe (-1) $ findIndex (isPrefixOf pat) (tails str)

{-
  查找子串在序列中所有位置，返回列表
>>> findAll "bc" "abcbcd"
[1,3]
-}
findAll :: (Eq a) => [a] -> [a] -> [Int]
findAll pat str = findIndices (isPrefixOf pat) (tails str)

{-
  'countOfPat'返回序列中指定子序列出现次数
>>> countOfPat "bc" "abcbcd"
2
-}
countOfPat :: Eq a => [a] -> [a] -> Int
countOfPat pat str = length $ filter (isPrefixOf pat) (tails str)

{-|
  函数'replaceNth'改变一维列表中元素的值
  @input 'xs'      一维列表
  @input 'n'       下标
  @input 'newVal'  改变后的值
  @output    将'xs!!n!!'改变为'newVal'后新的列表
-}
replaceNth :: [a] -> Int -> a -> [a]
replaceNth [] _ _  = []
replaceNth (x:xs) n newVal
 | n == 0 = newVal:xs
 | otherwise = x:replaceNth xs (n-1) newVal

{-|
  函数'change_elem'改变二维列表中元素的值
  @input 'xs'     二维数组
  @input '(x,y)'  坐标,从(1,1)开始
  @input 'x'      改变后的值
  @output    将'xs!!row!!col'改变为'x'后新的二维列表
-}
change_elem :: [[a]] -> (Int,Int) -> a -> [[a]]
change_elem xs (row,col) x =
  let row_to_replace = xs !! row
      modified_row = replaceNth row_to_replace col x
  in replaceNth xs row modified_row

{-
  从二维数组的行、列、主次对角线搜索一个pattern
  返回一个List：
  []               代表未搜索到结果
  [开始坐标,方向]    代表找到结果
  例如：["(1,1)","row"] 
  方向有 row col diag diag' 四种结果
-}
search_board :: [[Int]] -> [Int] -> [[String]]
search_board board pat =
  let rows = search_rows 0 pat board
      cols = search_cols 0 pat board
      diags = search_diags 0 pat board
      diags' = search_diags' 0 pat board
  in  map (\row -> [show row,"row"]) rows ++
      map (\col -> [show col,"col"]) cols ++
      map (\diag -> [show diag,"diag"]) diags ++
      map (\diag' -> [show diag',"diag'"]) diags'


----------------------
----- Help-Func ------
----------------------

-- 依次判断棋盘的所有行，看有无pattern
search_rows ::  Int -> [Int] -> [[Int]] -> [(Int,Int)]
search_rows _ _ [] = []
search_rows row pat (x:xs) = map (\col -> (row,col)) cols ++
                             search_rows (row+1) pat xs
  where cols = findAll pat x

-- 依次判断棋盘的所有列，看有无pattern  
search_cols ::  Int -> [Int] -> [[Int]] -> [(Int,Int)]
search_cols row pat = map adjust . (search_rows row pat) . transpose
  where
    adjust (x,y) = (y,x)


-- 依次判断棋盘的主对角线，看有无pattern
search_diags ::  Int -> [Int] -> [[Int]] -> [(Int,Int)]
search_diags row pat board = map adjust . (search_rows row pat) . diagonals $ board
  where
    n = length board
    adjust (-1,-1) = (-1,-1)
    adjust (x,y) = if x < n
                   then (x-y,y)
                   else (n-1-y,x-n+1+y)

search_diags' ::  Int -> [Int] -> [[Int]] -> [(Int,Int)]
search_diags' row pat board = map adjust . (search_rows row pat) . diagonals' $ board
  where
    n = length board
    adjust (-1,-1) = (-1,-1)
    adjust (x,y) = if x < n
                   then (x-y,n-1-y)
                   else (n-1-y,n+n-2-x-y)


