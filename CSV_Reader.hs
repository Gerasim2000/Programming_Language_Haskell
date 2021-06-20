module CSV_Reader where
import Control.Exception
import System.IO
import Data.Char ( isSpace )
--import Data.Text
import System.Directory

fileReadCsv :: String -> IO [[String]]
fileReadCsv str = do
  b <- doesFileExist st
  if b then (do read) else error ("Error - CSV file name \"" ++ st ++ "\" not found!")  -- error check if file is not found
      where st = removeQuotes str ++ ".csv"                                               -- removes quotes from parsing and adds .csv
            b = doesFileExist st
            read = fileReadCsv' st

fileReadCsv' :: String -> IO [[String]]
fileReadCsv' str = do
  text <- readFile str
  let ls = lines text
  let cells = map (splitOn ',') ls
  let nowhitespace = map (map removeWhitespace) cells
  return nowhitespace

--"example" -> example
removeQuotes :: String -> String
removeQuotes str = drop 1 (take (length str-1) str)

--"   example    " -> "example"
removeWhitespace :: String -> String
removeWhitespace str = reverse (dropWhile isSpace (reverse (dropWhile isSpace str)))

--SplitOn function from lectures
splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c ls = takeWhile (/=c) ls : splitOn' c (dropWhile (/=c) ls)
  where splitOn' c [] = []
        splitOn' c [x] | x==c = [[]]
        splitOn' c (x:xs) | x==c = splitOn c xs
                          | otherwise = []