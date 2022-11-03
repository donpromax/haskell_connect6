module Boards.Graphic (Picture
                      ,printPicture
                      ,flipV
                      ,flipH
                      ,above
                      ,sideByside) where
  
----------------------
-------- Data --------
----------------------

type Picture = [String]
                               
----------------------
------ Funcions ------
----------------------

-- 图形显示
printPicture :: Picture -> IO ()
printPicture pic = putStr $ unlines pic

-- 垂直翻转
flipV :: Picture -> Picture
flipV = reverse 

-- 水平翻转
flipH :: Picture -> Picture
flipH pic = [reverse line | line <- pic]

-- 上下拼接
above :: Picture -> Picture -> Picture
above [] q = q
above p [] = p
above p q = p ++ q

-- 水平拼接
sideByside :: Picture -> Picture -> Picture
sideByside [] q = q
sideByside p [] = p
sideByside p q  = [p ++ q | (p,q) <- zip p q]