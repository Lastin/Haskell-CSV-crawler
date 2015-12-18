-- |Module provides functions for parsing between strings, sqlValue, Day and Row data type
module CSVParser where

import Data.List.Split
import Data.Time
import Database.HDBC.SqlValue

-- |Custom data type to which rows from CSV are parsed
data Row = Row { date :: Day,
                 high :: Double,
                 low  :: Double
               } deriving (Show)

-- |Parses Row custom data type into array of SQL values containing the same data
rowToSql :: Row -> [SqlValue]
rowToSql (Row d h l) = [toSql d, toSql h, toSql l]

-- |Formats rows fetched from SQL for printing.
formatSqlRow :: [SqlValue] -> String
formatSqlRow c = a!!0 ++ (replicate (17 - (length $ a!!0)) ' ') ++ "|" ++ a!!1 ++ "\n----------------------------------"
                 where a = map (fromSql :: SqlValue -> String) c

-- |Formats rows fetched from SQL for comparing function
formatSqlRow' :: [SqlValue] -> String
formatSqlRow' c = "   " ++ (splitOn " " (a!!0) !! 0) ++ "   |" ++ wrapInSpaces (a!!1) ++ "|" ++ wrapInSpaces (a!!2)
                  where a = map (fromSql :: SqlValue -> String) c

-- |Adds spaces before and after string to format output
wrapInSpaces :: String -> String
wrapInSpaces s = replicate (15 - (length s)) ' ' ++ s ++ replicate 5 ' '

-- |Converts date data format from CSV into Day type
stringToDate :: String -> Day
stringToDate s = fromGregorian (toInteger $ t!!0) (t!!1) (t!!2)
                 where t = map (read :: String -> Int) $ splitOn "-" s

-- |Parses array of strings into object of type Row
createRow :: [String] -> Row
createRow s = Row {date = (stringToDate $ s!!0), high = read (s!!2), low = read (s!!3)}

-- |Converts CSV in string format into array of Rows
csvToRows :: String -> [Row]
csvToRows csv = map (createRow . splitOn ",") $ tail $ lines csv
