corr <- function(directory, threshold = 0) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'threshold' is a numeric vector of length 1 indicating the
	## number of completely  observed observations (on all
	## variables) required to complete the correlation  between
	## nitrate and sulfate; the default is 0
	
	## Return  a numeric  vector of correlations
	## NOTE: Do not round the result!

	rstl <- c(0)
	rcount = 0
	for(i in 1:332) {
		## Compose partial filename. Add 3 leading 0's to id.
		szId <- sprintf("%03d", i)
		filename <- paste(directory, "\\", szId, ".csv", sep = "")
		
		## Read all records in the file
		allData <- read.table(filename, header=TRUE, sep=",")
		
		## Use only columns sulfate and nitrate
		subsetData <- allData[ , 2:3]
		good <- complete.cases(subsetData)
				
		## Get good data
		a <- subsetData[good, ]
		
		## I will use the default method = "pearson"
		if ( as.numeric(nrow(a)) >= threshold) {
			rcount = rcount+1
			rstl[rcount] <- cor(a[ , 1], a[ , 2])
		}
		
	}
	return(rstl)
}