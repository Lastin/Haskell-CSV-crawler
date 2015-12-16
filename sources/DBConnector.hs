module DBConnector where

import Data.Maybe
import CSVParser
import Database.HDBC
import Database.HDBC.Sqlite3

createDB :: IO ()
createDB =
   do conn <- connectSqlite3 "stocks.db"
      run conn "CREATE TABLE IF NOT EXISTS companies (company_id INTEGER PRIMARY KEY, company_name TEXT UNIQUE NOT NULL)" []
      run conn "CREATE TABLE IF NOT EXISTS stocks (company_id INTEGER, date DATETIME UNIQUE, high DOUBLE, low DOUBLE, FOREIGN KEY(company_id) REFERENCES companies(company_id))" []
      commit conn

dropTables :: IO ()
dropTables =
   do conn <- connectSqlite3 "stocks.db"
      run conn "DROP TABLE IF EXISTS stocks; DROP TABLE IF EXISTS companies" []
      commit conn


storeRows :: String -> [Row] -> IO()
storeRows _ [] = return ()
storeRows c rows = 
   do conn <- connectSqlite3 "stocks.db"
      quickQuery' conn "INSERT OR IGNORE INTO companies (company_name) VALUES (?)" [toSql c]
      stmt <- prepare conn "SELECT company_id FROM companies WHERE company_name = ?"
      execute stmt [toSql c]
      result <- fetchRow stmt
      case result of
         Just id -> do stmtStocks <- prepare conn $ "INSERT OR IGNORE INTO stocks (company_id, date, high, low) VALUES (?,?,?,?)"
                       executeMany stmtStocks $ map (id ++) (map (rowToSql) rows)
      commit conn

printHeader :: IO ()
printHeader =
   do putStrLn "     Company     |     Highest    "
      putStrLn "----------------------------------"

printHighest :: IO ()
printHighest =
   do conn <- connectSqlite3 "stocks.db"
      stmt <- prepare conn "SELECT company_name, max(high) FROM stocks JOIN companies ON stocks.company_id = companies.company_id GROUP BY company_name ORDER BY high DESC"
      execute stmt []
      result <- fetchAllRows stmt
      printHeader
      mapM_ (putStrLn . formatSqlRow) result

printLowest :: IO()
printLowest =
      do conn <- connectSqlite3 "stocks.db"
         stmt <- prepare conn "SELECT company_name, min(low) FROM stocks JOIN companies ON stocks.company_id = companies.company_id GROUP BY company_name ORDER BY low ASC"
         execute stmt []
         result <- fetchAllRows stmt
         printHeader
         mapM_ (putStrLn . formatSqlRow) result

getCompanies :: IO [String]
getCompanies =
   do conn <- connectSqlite3 "stocks.db"
      result <- quickQuery' conn "SELECT company_name FROM companies" []
      return $ map (fromSql . head) result
