module Records.CsvParser (parseCSV) where
  
-- file: ch16/csv9.hs
import Text.ParserCombinators.Parsec

----------------------
-------- Data --------
----------------------

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell =
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol = try (string "\n\r") 
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"
  
----------------------
------ Funcions ------
---------------------- 

{-|
  函数'parseCSV'解析.csv文件
  @input    文件相对路径
  @output   [[String]]格式的解析结果
-}
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input


parseCSVFile :: FilePath -> IO ()
parseCSVFile filename =
    do c <- readFile filename
       case parse csvFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e     
            Right r -> mapM_ print r