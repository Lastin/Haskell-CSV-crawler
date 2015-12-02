module CSVDownloader where

import Network.HTTP
import Network.HTTP.Base
import Network.URI
import Data.Maybe

--csv_url = ["http://real-chart.finance.yahoo.com/table.csv?s=%", "&ignore=.csv"]
csv_url = "http://real-chart.finance.yahoo.com/table.csv?s="

downloadCSV :: String -> IO String
downloadCSV company = 
   do print uri
      print request
      resp <- simpleHTTP request
      case resp of
         Left x -> return $ "Error connecting: " ++ show x
         Right r ->
            case rspCode r of
               (2,_,_) -> return $ rspBody r
               _ -> return $ show r
   where request = Request {rqURI = uri, rqMethod = GET, rqHeaders = [], rqBody = ""}
         uri = fromJust $ parseURI (csv_url ++ (urlEncode company))
