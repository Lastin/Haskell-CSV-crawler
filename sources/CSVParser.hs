module CSVParser where
import Data.List.Split
import Data.Time

data Row = Row { company_name	:: String,
					  date         :: Day,
				     high      	:: Double,
				     low       	:: Double
			  } deriving (Show)

stringToDate :: String -> Day
stringToDate s = fromGregorian (t!!0) (t!!1) (t!!2)
					  where t = splitOn "-" s

createRow :: String -> [String] -> Row
createRow c s = Row {company_name = c, date = s!!0, high = read (s!!2), low = read (!!3)}

csvToRows :: String -> String -> [Row]
csvToRows c csv = map ((createRow c) . (splitOn ",")) $ tail . lines csv
