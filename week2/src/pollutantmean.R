##Part 1 : Function to calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors.

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
 
## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".
    
## 'id' is an integer vector indicating the monitor ID numbers
## to be used
    
## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)

pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
	
	# validate: is a valid directory 
	if(!file.exists(directory)){
		stop(paste(directory ," is not a valid directory"))
	}
	# validate: directory contains specdata csv files
	allFiles <- list.files(pattern = "\\.csv$")
	if(length(allFiles)<=0){
		stop(paste("No csv files in this directory [","directory","]"))
	}
	
	#init vector for pollutant data
	meanVector <- c()
	
	for(i in id){
	    # read data from csv file
		currentFile <- read.csv(paste(directory,"/",as.character(allFiles[i]),sep=""),header=TRUE,sep=",")
		# Get NAs
		naRemoved <- currentFile[!is.na(currentFile[, pollutant]), pollutant]
		# create non NAs vector
		meanVector <- c(meanVector, naRemoved)
	}
	result <- mean(meanVector)
	return(round(result, 3)) 
}

pollutantmean("e:/learning/codebase/datasciencecoursera/week2/specdata", "sulfate", 1:10) == 4.064
pollutantmean("e:/learning/codebase/datasciencecoursera/week2/specdata", "nitrate", 70:72) == 1.706
pollutantmean("e:/learning/codebase/datasciencecoursera/week2/specdata", "nitrate", 23) == 1.281