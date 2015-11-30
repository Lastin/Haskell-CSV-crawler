import DBConnector
import CSVDownloader
import System.Environment

-- |The main function provides five different functionalities:
--
--  [@create@] will create the database csvs.db
--
--  [@saved@] simply prints all the stocks currently stored on the database
--
--  [@unfold@] will download and parse all pages on the database, storing all found links
--
--  [@show url@] will download and display the contents of the csv for the given company id
--
--  [@crawl url@] will download a given page, parse the links and store on the database  
main = do args <- getArgs
          case args of
             ["create"] -> createDB	
             ["saved"] -> printStocks	
             ["unfold"] -> unfoldDB
             ["show", url] ->
             	do urlText <- downloadURL url
             	   print urlText
             ["crawl", url] ->
             	do urlText <- downloadURL url
             	   let urls = parseURLs urlText
             	   storeURLs urls
             _ -> syntaxError

syntaxError = putStrLn 
  "Usage: Crawler command [args]\n\
  \\n\
  \create           Create database urls.db\n\
  \show url         Shows contents of given URL\n\
  \saved            List urls on database\n\
  \crawl url        Gather urls and store in database\n\
  \unfold           Crawl each of the saved URLs\n"
