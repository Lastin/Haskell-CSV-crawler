module DBConnector where

import CSVParser
import Database.HDBC
import Database.HDBC.Sqlite3

createDB :: IO ()
createDB =
   do conn <- connectSqlite3 "stocks.db"
      run conn "CREATE TABLE IF NOT EXISTS companies (company_id INTEGER PRIMARY KEY, company_name TEXT UNIQUE NOT NULL)" []
      run conn "CREATE TABLE IF NOT EXISTS stocks (company_id INTEGER, date DATETIME, high DOUBLE, low DOUBLE, FOREIGN KEY(company_id) REFERENCES companies(company_id))" []
      commit conn

dropTables :: IO ()
dropTables =
   do conn <- connectSqlite3 "stocks.db"
      run conn "DROP TABLE companies; DROP TABLE stocks" []
      commit conn


storeRows :: String -> [Row] -> IO ()
storeRows _ [] = return ()
storeRows c rows = 
   do conn <- connectSqlite3 "stocks.db"
      stmtCompany <- prepare conn "INSERT OR IGNORE INTO companies (company_name) VALUES (?)"
      execute stmtCompany [toSql c]
      stmtStocks <- prepare conn $ "INSERT OR IGNORE INTO csvs (company_id, date, high, low)\
                                    \SELECT company_id,?,?,? FROM companies WHERE company_name = " ++ c
      --TODO: execute stmtStocks
      --executeMany stmtStocks 
      commit conn
