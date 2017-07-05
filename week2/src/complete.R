## Part 2 : Function to reports the number of completely observed cases in each data file.
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
    
## 'id' is an integer vector indicating the monitor ID numbers
## to be used
   
## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
	
complete <- function(directory, id = 1:332) {
	
	# validate: is a valid directory 
	if(!file.exists(directory)){
		stop(paste(directory ," is not a valid directory"))
	}
	# validate: directory contains specdata csv files
	allFiles <- list.files(pattern = "\\.csv$")
	if(length(allFiles)<=0){
		stop(paste("No csv files in this directory [","directory","]"))
	}
	counter <- 1 
	completeData <- rep(0, length(id))
	for(i in id){
	    # read data from csv file
		currentFile <- read.csv(paste(directory,"/",as.character(allFiles[i]),sep=""),header=TRUE,sep=",")
		completeData[counter] <- sum(complete.cases(currentFile))
		counter <- counter + 1
	}
	 result <- data.frame(id = id, nobs = completeData)
	 return(result)
}

complete("e:/learning/codebase/datasciencecoursera/week2/specdata", 1)
complete("e:/learning/codebase/datasciencecoursera/week2/specdata", c(2, 4, 8, 10, 12))
complete("e:/learning/codebase/datasciencecoursera/week2/specdata", 30:25)
complete("e:/learning/codebase/datasciencecoursera/week2/specdata", 3)
