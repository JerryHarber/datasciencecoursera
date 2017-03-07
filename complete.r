complete <- function(directory, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	## Return a data frame of the form:
	## id nobs
	## 1 117
	## 2 1041
	## ...
	## where  'id'  is the monitor ID number and 'nobs' is the
	## number of completed cases
	
	## Create an empty data frame for holding the counts
	b <- data.frame(id = 0, nobs = 0)
	rowcounter = 0
	for(i in id) {
		## Compose partial filename. Add 3 leading 0's to id.
		szId <- sprintf("%03d", i)
		filename <- paste(directory, "\\", szId, ".csv", sep = "")
		
		## Read all records in the file
		allData <- read.table(filename, header=TRUE, sep=",")
		subsetData <- allData[ , 2:3]
		good <- complete.cases(subsetData)
				
		## Get good data
		a <- subsetData[good, ]
		rowcounter = rowcounter + 1
		b[rowcounter, 1] = i
		b[rowcounter, 2] = nrow(a)
	}
	return(b)
	
}