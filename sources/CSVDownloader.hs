-- |Module performs HTTP requests
module CSVDownloader where

import Network.HTTP
import Network.HTTP.Base
import Network.URI
import Data.Maybe

-- |Function returns URL to CSV with stocks prices from Yahoo finance
csvUrl :: String -> String
csvUrl company = "http://real-chart.finance.yahoo.com/table.csv?s=" ++ company

-- |Downloads the csv from the url concatenated with company name. Returns csv as string if successful, or as error message otherwise.
downloadCSV :: String -> IO String
downloadCSV company = 
   do resp <- simpleHTTP request
      case resp of
         Left x -> return $ "Error connecting: " ++ show x
         Right r ->
            case rspCode r of
               (2,_,_) -> return $ rspBody r
               _ -> return $ show r
   where request = Request {rqURI = uri, rqMethod = GET, rqHeaders = [], rqBody = ""}
         uri = fromJust $ parseURI $ csvUrl company
