## Part 3 : Function to  calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold.
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
    
## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0
    
## Return a numeric vector of correlations


corr <- function(directory, threshold = 0) {
	
	# validate: is a valid directory 
	if(!file.exists(directory)){
		stop(paste(directory ," is not a valid directory"))
	}
	# validate: directory contains specdata csv files
	allFiles <- list.files(pattern = "\\.csv$")
	if(length(allFiles)<=0){
		stop(paste("No csv files in this directory [","directory","]"))
	}
	# get the complete table
    completeTable <- complete(directory, 1:332)
    nobs <- completeTable$nobs
	
    # find the valid ids
    ids <- completeTable$id[nobs > threshold]
    corrVector <- rep(0,  length(ids))
	
	
 
   counter <- 1 
    for(i in ids) {
		currentFile <- read.csv(paste(directory,"/",as.character(allFiles[i]),sep=""),header=TRUE,sep=",")
        corrVector[counter] <- cor(currentFile$sulfate, currentFile$nitrate, use="complete.obs")
        counter <- counter + 1
    }
    result <- corrVector
	return(result) 
}

cr <- corr("e:/learning/codebase/datasciencecoursera/week2/specdata", 150)
head(cr)
summary(cr)
cr <- corr("e:/learning/codebase/datasciencecoursera/week2/specdata", 400)
head(cr)
summary(cr)
cr <- corr("e:/learning/codebase/datasciencecoursera/week2/specdata", 5000)
summary(cr)
length(cr)
cr <- corr("e:/learning/codebase/datasciencecoursera/week2/specdata")
summary(cr)
length(cr)