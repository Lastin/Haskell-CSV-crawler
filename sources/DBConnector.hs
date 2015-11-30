-- |Module containing functions that operate on the database csvs.db
module DBconnector where

import CSVdownloader
import Database.HDBC
import Database.HDBC.Sqlite3


-- |Method to create the database csvs.db
-- It uses connectSqlite3 from Database.HDBC.Sqlite3
createDB :: IO ()
createDB = do conn <- connectSqlite3 "csvs.db"
              run conn "CREATE TABLE companies IF NOT EXIST (company_id TEXT PRIMARY KEY, company_name TEXT)"
				  run conn "CREATE TABLE csvs IF NOT EXIST (\
									company_id TEXT,\
									date DATETIME,\
									open DOUBLE,\
									high DOUBLE,\
									low DOUBLE,\
									close DOUBLE,\
									volume DOUBLE,\
									adj_close DOUBLE,\
									FOREIGN KEY() REFERENCES companies(company_id))" []
              commit conn

-- |Method to store the data from csv file to the database (table csvs)
storeCSVs :: [Row] -- ^ List of rows to be stored on the database
          -> IO ()
storeCSVs [] = return ()
storeCSVs xs = 
     do conn <- connectSqlite3 "csvs.db"
        stmt <- prepare conn "INSERT IGNORE INTO csvs (company, date, open, high, low, close, volume, adj_close) VALUES (????????)"
        executeMany stmt (map (\x -> [toSql x]) xs)
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
