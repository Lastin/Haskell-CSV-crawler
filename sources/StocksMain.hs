import DBConnector
import CSVDownloader
import CSVParser
import System.Environment

main = do args <- getArgs
          case args of
             ["create"] -> createDB
             ["drop"] -> dropTables
             ["save", company] -> do saveCompany company
             ["update"] -> do companies <- getCompanies
                              mapM_ saveCompany companies
             ["highest"] -> printHighest
             ["lowest"] -> printLowest
             _ -> syntaxError

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
  \highest          Prints highest stock price of each company\n\
  \lowest           Prints lowest stock price of each company\n"
