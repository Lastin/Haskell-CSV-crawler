module DBConnector where

import CSVParser
import CSVDownloader
import Database.HDBC
import Database.HDBC.Sqlite3

createDB :: IO ()
createDB = do conn <- connectSqlite3 "stocks.db"
              run conn "CREATE TABLE companies IF NOT EXIST (company_id INTEGER PRIMARY KEY, company_name TEXT UNIQUE NOT NULL)"
				  run conn "CREATE TABLE stocks IF NOT EXIST (\
									company_id INTEGER,\
									date DATETIME,\
									high DOUBLE,\
									low DOUBLE,\
									FOREIGN KEY(company_id) REFERENCES companies(company_id))" []
              commit conn

storeRows :: [Row] -> IO ()
storeRows [] = return ()
storeRows xs = 
     do conn <- connectSqlite3 "csvs.db"
		  stmtCompanies <- prepare conn "INSERT OR IGNORE INTO companies (company_name) VALUES (?)"
		  executeMany stmtCompanies (map (\x -> [toSql (company_name x)]) xs)
        stmtStocks <- prepare conn "INSERT OR IGNORE INTO csvs (company_id, date, high, low)\
												SELECT company_id,?,?,?\
												FROM companies WHERE company_name = ?"
		  --TODO: execute stmtStocks
        commit conn
