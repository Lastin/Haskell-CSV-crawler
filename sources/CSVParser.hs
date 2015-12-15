module CSVParser where
import Data.List.Split
import Data.Time
import Database.HDBC.SqlValue

data Row = Row { date :: Day,
                 high :: Double,
                 low  :: Double
               } deriving (Show)

rowToSql :: Row -> [SqlValue]
rowToSql (Row d h l) = [toSql d, toSql h, toSql l]

stringToDate :: String -> Day
stringToDate s = fromGregorian (toInteger $ t!!0) (t!!1) (t!!2)
                 where t = map (read :: String -> Int) $ splitOn "-" s

createRow :: [String] -> Row
createRow s = Row {date = (stringToDate $ s!!0), high = read (s!!2), low = read (s!!3)}

csvToRows :: String -> [Row]
csvToRows csv = map (createRow . splitOn ",") $ tail $ lines csv
