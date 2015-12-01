module CSVParser where



data Row = Row { company_name	:: String,
					  date         :: Date,
					  open     		:: Double,
				     high      	:: Double,
				     low       	:: Double,
				     close     	:: Double,
				     volume    	:: Double,
				     adj_close 	:: Double
				} deriving (Show)


