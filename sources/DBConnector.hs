-- |Module containing functions that operate on the database csvs.db
module DBConnector where

import CSVParser
import CSVDownloader
import Database.HDBC
import Database.HDBC.Sqlite3


-- |Method to create the database stocks.db
-- It uses connectSqlite3 from Database.HDBC.Sqlite3
createDB :: IO ()
createDB = do conn <- connectSqlite3 "stocks.db"
              run conn "CREATE TABLE companies IF NOT EXIST (company_id INTEGER PRIMARY KEY, company_name TEXT UNIQUE NOT NULL)"
				  run conn "CREATE TABLE stocks IF NOT EXIST (\
									company_id INTEGER,\
									date DATETIME,\
									open DOUBLE,\
									high DOUBLE,\
									low DOUBLE,\
									close DOUBLE,\
									volume DOUBLE,\
									adj_close DOUBLE,\
									FOREIGN KEY(company_id) REFERENCES companies(company_id))" []
              commit conn

-- |Method to store the data from csv file to the database (table csvs)
storeRows :: [Row] -- ^ List of rows to be stored on the database
          -> IO ()
storeRows [] = return ()
storeRows xs = 
     do conn <- connectSqlite3 "csvs.db"
		  stmtCompanies <- prepare conn "INSERT OR IGNORE INTO companies (company_name) VALUES (?)"
		  executeMany stmtCompanies (map (\x -> [toSql (company x)]) xs)
        stmtStocks <- prepare conn "INSERT OR IGNORE INTO csvs (company_id, date, open, high, low, close, volume, adj_close)\
												SELECT company_id,?,?,?,?,?,?,?\
												FROM companies WHERE company_name = ?"
--TODO map custom data into statement
		  executeMany stmtStocks (map (\x -> [toSql x]) xs)
        commit conn        

-- |Method to display all the URLs on the database. It uses getURLs.
printURLs :: IO ()
printURLs = do urls <- getURLs
               mapM_ print urls

-- |Method to retrieve all the URLs on the database.
getURLs :: IO [URL]
getURLs = do conn <- connectSqlite3 "urls.db"
             res <- quickQuery' conn "SELECT url FROM urls" []
             return $ map fromSql (map head res)

-- |Method to retrive all the URLs from the database, parse and extract new links
-- from these, and store those links on the database. This method could be made more
-- efficient by remembering which URLs have already been processed, so that next
-- time unfoldDB is called only newly retrieved urls should be processed.
unfoldDB :: IO ()
unfoldDB = do urls <- getURLs
              process urls

-- |Method to donwload a given list of URLs, extract new links and store these on the database.
process :: [URL]  -- ^ List of URLs to be processed
        -> IO ()
process [] = return ()
process (x:xs) = do print $ "Processing : " ++ x
                    urlContent <- downloadURL x
                    storeURLs (parseURLs urlContent)
                    process xs
