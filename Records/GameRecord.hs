{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Records.GameRecord (readblack
                          ,readwhite
                          ,readcontent
                          
                          ,record2player
                          ,record2line
                          
                          ,reformatRecords
                          ,showRecord
                          ,getRecord) where
  
import Utils
import Records.CsvParser
import Records.Times 


----------------------
-------- Data --------
----------------------
type Records = [[String]]
type SingleRecord = [String]
type Solution = [(Int,Int)]

----------------------
------ Funcions ------
----------------------
reformatRecords :: Records -> Records
reformatRecords = map (plus_one 2) .
                  map (plus_one 3)

{- 
  在终端打印SingleRecord 
-}
showRecord :: SingleRecord -> String
showRecord r = (if last order == 'b' then "黑方" else "白方") ++
               "第" ++ head order:"手:" ++
               (if length r == 3 
                then format (last r) 
                else format (r!!2) ++ ", " ++ format (r!!3)
               ) ++ "\n"             
  where
    order = r!!1
    format s = let (x,y) = read s :: (Int,Int)
               in show (x+1,y+1)
{-
  根据秒数，步数，解。生成record
-}
getRecord :: String -> Int -> Solution -> SingleRecord
getRecord time n solution =
  let (x1,y1) = head solution
      (x2,y2) = last solution
      move1   = show (x1+1,y1+1) 
      move2   = show (x2+1,y2+1) 
  in  if move1 == move2
      then [time,show n,move1]
      else [time,show n,move1,move2]             
                       
{- 读black.csv并在步数前面加上b -}
readblack :: String -> Records
readblack = map (\xs -> replaceNth xs 1 (xs!!1 ++ "b")) . 
            readcontent        
  

{- 读white.csv并在步数前面加上w -}
readwhite :: String -> Records
readwhite = map (\xs -> replaceNth xs 1 (xs!!1 ++ "w")) . 
            readcontent
  

{- 输入一条记录 返回白子：2 黑子：1 -}
record2player :: SingleRecord -> Int
record2player sr = if sr !! 1 !! 1 == 'w'
                   then 2
                   else 1
                   
{- 单条记录的转换为csv格式字符串的方法 -}
record2line :: SingleRecord -> String
record2line = init . record2string
  where
    record2string [] = ""
    record2string (x:xs) = (if x!!0 /= '(' then x else show x)
                       ++ ',':record2string xs

----------------------
----- Help-Func ------
----------------------
-- 将record的第i项元组(+1,+1)
minus_one :: Int -> SingleRecord -> SingleRecord
minus_one n sr = replaceNth sr n (show (x-1,y-1))
  where
    (x,y) = read (sr!!n) :: (Int,Int)
    
-- 将record的第i项元组(+1,+1)
plus_one :: Int -> SingleRecord -> SingleRecord
plus_one n sr = replaceNth sr n (show (x+1,y+1))
  where
    (x,y) = read (sr!!n) :: (Int,Int)
    
-- 读csv的plain txt并调用用parser转换为[[String]]
readcontent :: String -> Records
readcontent  = map (minus_one 2) .
               map (minus_one 3) .
               readcontent'
  where 
    readcontent' content = case parseCSV content of
          Left l -> []
          Right r -> r
