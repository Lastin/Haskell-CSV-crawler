import DBConnector
import CSVDownloader
import System.Environment

main = do args <- getArgs
          case args of
             ["create"] -> createDB	
             ["saved"] -> printStocks
             _ -> syntaxError

syntaxError = putStrLn 
  "Usage: StocksMain command [args]\n\
  \\n\
  \create           Create database urls.db\n\
  \show url         Shows contents of given URL\n\
  \saved            List urls on database\n\
  \crawl url        Gather urls and store in database\n\
  \unfold           Crawl each of the saved URLs\n"
