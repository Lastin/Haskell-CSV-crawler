import DBConnector
import CSVDownloader
import CSVParser
import System.Environment

main = do args <- getArgs
          case args of
             ["create"] -> createDB
             ["drop"] -> dropTables
             ["save", company] ->
               do csv <- downloadCSV company
                  let rows = csvToRows csv
                  storeRows company rows
             ["highest"] -> printHighest
             _ -> syntaxError

syntaxError = putStrLn 
  "Usage: StocksMain command [args]\n\
  \\n\
  \create           Create database stocks.db\n\
  \drop             Drop tables in database stocks.db\n\
  \save company     Saves csv into the database\n\
  \highest          Prints highest 'high' for each company"
