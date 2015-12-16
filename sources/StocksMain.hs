import DBConnector
import CSVDownloader
import CSVParser
import System.Environment

-- |The main function provides access to functionalities of the program
--[@create@] creates the database to store csv
--[@drop@] drops the tables in the database
--[@save company@] saves csv of the `company` into database
--[@update@] updates the stocks of existing companies 
--[@highest@] prints highest stocks prices of each company in database
--[@lowest@]  prints lowest stock prices of each company in database

main = do args <- getArgs
          case args of
             ["create"] -> createDB
             ["drop"] -> dropTables
             ["save", company] -> do saveCompany company
             ["update"] -> do companies <- getCompanies
                              mapM_ saveCompany companies
             ["companies"] -> do companies <- getCompanies
                                 mapM_ putStrLn companies
             ["highest"] -> printHighest
             ["lowest"] -> printLowest
             _ -> syntaxError

-- |Downloads csv of a company passed to it, then parses csv into custom data type. Then it stores parsed data in the database
saveCompany :: String -> IO ()
saveCompany company = do csv <- downloadCSV company
                         let rows = csvToRows csv
                         storeRows company rows

syntaxError = putStrLn 
  "Usage: StocksMain command [args]\n\
  \\n\
  \create           Create database stocks.db\n\
  \drop             Drop tables in database stocks.db\n\
  \update           Updates the stocks for existing companies\n\
  \save company     Saves csv into the database\n\
  \companies        Prints companies saved in the database\n\
  \highest          Prints highest stock price of each company\n\
  \lowest           Prints lowest stock price of each company\n"
