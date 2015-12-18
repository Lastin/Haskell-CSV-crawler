-- |Module provides connectivity functions with MySQL database
module DBConnector where

import Data.Maybe
import CSVParser
import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Database.HDBC.MySQL as My

-- |Returns connection to university MySQL database
connectToMySql :: IO My.Connection
connectToMySql = 
   do My.connectMySQL My.defaultMySQLConnectInfo {
         My.mysqlHost     = "dbprojects.eecs.qmul.ac.uk",
         My.mysqlUser     = "mm306",
         My.mysqlPassword = "fNZOoc8FMovzo",
         My.mysqlDatabase = "mm306"
   }

-- |Creates tables in the stocks.db database
createDB :: IO ()
createDB =
   do conn <- connectToMySql
      run conn "CREATE TABLE IF NOT EXISTS companies (company_id INTEGER PRIMARY KEY AUTO_INCREMENT, company_name VARCHAR(255) UNIQUE NOT NULL)" []
      run conn (unlines ["CREATE TABLE IF NOT EXISTS stocks (",
                                                     "company_id INTEGER, ",
                                                     "date DATETIME, ",
                                                     "high DOUBLE, ",
                                                     "low DOUBLE, ",
                                                     "FOREIGN KEY(company_id) REFERENCES companies(company_id), ",
                                                     "UNIQUE KEY `uniq_comp_stock` (`company_id`, `date`))"]) []
      commit conn
      disconnect conn

-- |Drops the tables stocks and companies from stocks.db database
dropTables :: IO ()
dropTables =
   do conn <- connectToMySql
      run conn "DROP TABLE IF EXISTS stocks CASCADE" []
      run conn "DROP TABLE companies" []
      commit conn
      disconnect conn

-- |Takes company name and rows and inserts company and stock prices into database, ignoring existing values
storeRows :: String -> [Row] -> IO()
storeRows _ [] = return ()
storeRows c rows = 
   do conn <- connectToMySql
      run conn "INSERT IGNORE INTO companies (company_name) VALUES (?)" [toSql c]
      c_id <- quickQuery' conn "SELECT company_id FROM companies WHERE company_name = ?" [toSql c]
      stmtStocks <- prepare conn $ "INSERT IGNORE INTO stocks (company_id, date, high, low) VALUES (?,?,?,?)"
      executeMany stmtStocks $ map (head c_id ++) (map (rowToSql) rows)
      commit conn
      disconnect conn

-- |Function for printing header, used by @printHighest@ and @printLowest@ functions
printHeader :: IO ()
printHeader =
   do putStrLn "     Company     |     Highest    "
      putStrLn "----------------------------------"

-- |Queries and prints the maximum value of column high for each company
printHighest :: IO ()
printHighest =
   do conn <- connectToMySql
      stmt <- prepare conn "SELECT company_name, max(high) FROM stocks JOIN companies ON stocks.company_id = companies.company_id GROUP BY company_name ORDER BY high DESC"
      execute stmt []
      result <- fetchAllRows stmt
      printHeader
      mapM_ (putStrLn . formatSqlRow) result
      disconnect conn

-- |Queries and prints the minimum value of column low for each company
printLowest :: IO()
printLowest =
   do conn <- connectToMySql
      stmt <- prepare conn "SELECT company_name, min(low) FROM stocks JOIN companies ON stocks.company_id = companies.company_id GROUP BY company_name ORDER BY low ASC"
      execute stmt []
      result <- fetchAllRows stmt
      printHeader
      mapM_ (putStrLn . formatSqlRow) result
      disconnect conn

-- |Returns names of companies from the database
getCompanies :: IO [String]
getCompanies =
   do conn <- connectToMySql
      result <- quickQuery' conn "SELECT company_name FROM companies" []
      disconnect conn
      return $ map (fromSql . head) result

-- |Prints stock prices on same date for companies within selected date range
compareStocks :: String -> String -> String -> String -> String -> IO()
compareStocks col comp1 comp2 start end =
   do conn <- connectToMySql
      let query = "SELECT s1.date, s1."++col++", s2."++col++" FROM (SELECT stocks.company_id, date, "++col++" FROM stocks JOIN (SELECT company_name, company_id FROM companies WHERE company_name = ?) AS comp ON stocks.company_id = comp.company_id WHERE date >= ? AND date <= ?) AS s1 JOIN (SELECT stocks.company_id, date, "++col++" FROM stocks JOIN (SELECT company_name, company_id FROM companies WHERE company_name = ?) AS comp ON stocks.company_id = comp.company_id WHERE date >= ? AND date <= ?) AS s2 ON s1.date = s2.date"
      stmt <- prepare conn query
      execute stmt $ map toSql [comp1, start, end, comp2, start, end]
      result <- fetchAllRows stmt
      putStrLn $ "      DATE      |" ++ wrapInSpaces comp1 ++ "|" ++ wrapInSpaces comp2
      putStrLn $ replicate 68 '-'
      mapM_ (putStrLn . formatSqlRow') result
      disconnect conn
      
      
      
