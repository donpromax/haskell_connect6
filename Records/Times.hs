module Records.Times (Time
                     ,timeover
                     ,fromSeconds
                     ,plusSeconds) where

import Data.Char (isDigit)



----------------------
-------- Data --------
----------------------

data Time = Time {hour :: Int
                 ,minute :: Int
                 ,second :: Int }
                 
instance Show Time where
  show (Time hour minute second) = format hour ++ "/" ++ 
                                   format minute ++ "/" ++ 
                                   format second                             
    where
      format n = if n > 9
                 then (show n)
                 else ("0" ++ show n)
                                      
instance Read Time where
  readsPrec _ input =
    let (hours,rest1) = span isDigit input
        hour = read hours :: Int
        
        (c1:rest2) = rest1
        (mins,rest3) = splitAt 2 rest2
        minute = read mins :: Int
        
        (c2:rest4) = rest3
        (secs,rest5) = splitAt 2 rest4
        second = read secs :: Int
    in
      if c1 == '/' && 
         c2 == '/' &&
         all isDigit hours && 
         length hours == 2 &&
         all isDigit mins && 
         length mins == 2 &&
         all isDigit secs && 
         length secs == 2
      then [(newTime hour minute second,rest5)]
      else []
      
----------------------
------ Funcions ------
----------------------
fromSeconds :: Int -> String
fromSeconds s =
  let hours = s `div` 3600
      seconds' = s `mod` 3600
      minutes = seconds' `div` 60
      seconds = seconds' `mod` 60
  in show Time {hour = hours, minute = minutes, second = seconds}


{-|
  输入秒，返回格式化时间
-}
plusSeconds :: String -> Int -> String
plusSeconds timeStr secs =
  let time = read timeStr :: Time
      oldsecs = hour time * 60 * 60 + minute time * 60 + second time
      s = oldsecs + secs
      hours = s `div` 3600
      seconds' = s `mod` 3600
      minutes = seconds' `div` 60
      seconds = seconds' `mod` 60
  in show Time {hour = hours, minute = minutes, second = seconds}
  
{-|
  判断是否超时的函数
  @input 'newStr' 新时间的字符串格式
  @input 'oldStr' 老时间的字符串格式
  @output 'Bool'  相差大于5s输出True；否则False
-}
timeover :: String -> String -> Bool
timeover newStr oldStr = news - olds > 5
  where
    new = read newStr :: Time
    old = read oldStr :: Time
    news = hour new * 60 * 60 + minute new * 60 + second new
    olds = hour old * 60 * 60 + minute old * 60 + second old

----------------------
----- Help-Func ------
----------------------

newTime :: Int -> Int -> Int -> Time
newTime h m s
  | between 0 23 h && between 0 59 m && between 0 59 s = Time h m s
  | otherwise = error "newTime: hours must be in range 0-23 and minutes seconds 0-59"
    where between low high val = low <= val && val <= high