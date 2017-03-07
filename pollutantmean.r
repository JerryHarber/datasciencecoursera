pollutantmean <- function(directory, pollutant, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	
	## 'pollutant' is a character vector of length 1 indicating
	## the name of the pollutant for which we will calculate the
	## mean; either "sulfate" or "nitrate"
	
	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used
	
	## Return the mean of the pollutant across all monitors list
	## in the 'id' vector (ignoring NA values)
	## NOTE: Do not round the result!
	
	## Read files. Create am empty summation vectors
	sumx = 0
	sumn = 0
	
	for(i in id) {
		## Compose partial filename. Add 3 leading 0's to id.
		szId <- sprintf("%03d", i)
		filename <- paste(directory, "\\", szId, ".csv", sep = "")
		
		## Read all records in the file
		allData <- read.table(filename, header=TRUE, sep=",")
				
		## Get a subset of the data based on pollutant type
		subsetData <- allData[ , pollutant]
		bad = is.na(subsetData)
		good = subsetData[!bad]
		sumx = sum(good) + sumx
		sumn = sumn + length(good)
	}
	xbar = sumx / sumn
	## round(expectedvalue, 3)
	return(xbar)
}