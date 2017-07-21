# -----------------------------------------------------------------------------
#
# UCI HAR Dataset Analysis
#  Autor : Shibu Kumar Nair
# -----------------------------------------------------------------------------
library(reshape2)

#
# Function to download the data from web.
#
download_data <- function(downloadUrl,fileName) {
    
	os <- tolower(.Platform$OS.type)
	
	if(!file.exists(getwd())){
		stop(paste(getwd() ," is not a valid directory"))
	}
	
	if(os=="windows"){
		download.file(downloadUrl, paste("./",fileName))
	}else{
		download.file(downloadUrl, paste("./",fileName), method="curl")
	}
	
	if(grepl("^.*(.gz|.bz2|.tar|.zip|.tgz|.gzip|.7z)[[:space:]]*$", fileName)){
	   print(getwd())
		unzip(paste("./",fileName)) 
	 }
	
}

# -----------------------------------------------------------------------------
# Description:
#
# This function will perform the following steps on the UCI HAR Dataset 
# by downloaded from :-
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#      1. Merges the training and the test sets to create one data set.
#      2. Extracts only the measurements on the mean and standard deviation 
#         for each measurement.
#      3. Uses descriptive activity names to name the activities in the data 
#          set.
#      4. Appropriately labels the data set with descriptive variable names.
#      5. From the data set in step 4, creates a second, independent tidy data 
#         set with the average of each variable for each activity and each 
#         subject.
#
# Usage:
#
#     UCI_HAR_data_analysis()
#     
# Examples:
#
#     UCI_HAR_data_analysis()
#
# -----------------------------------------------------------------------------
UCI_HAR_data_analysis <- function() {

  rm(list = ls())
	
  WORKING_DIR =getwd();
  DOENLOAD_URL ="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  DATA_FILE_NAME = "UCI_HAR_dataset.zip"
  DATA_DIR ="./UCI HAR Dataset"
  
  #download_data(DOENLOAD_URL,DATA_FILE_NAME)
  
   # -----------------------------------------------------------------------------
   # 1. Merges the training and the test sets to create one data set.
   # -----------------------------------------------------------------------------

   # Read in the data from files
    featuresRowData     = read.table('./UCI HAR Dataset/features.txt',header=FALSE);             #read data from  features.txt
    activityTypeRowData = read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE);      #read data from activity_labels.txt
    subjectTrainRowData = read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE);  #read data from subject_train.txt
    xTrainRowData       = read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE);        #read data from x_train.txt
    yTrainRowData       = read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE);        #read data from y_train.txt
	subjectTestRowData  = read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE);    #read data from subject_test.txt
	xTestRowData        = read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE);          #read data from x_test.txt
	yTestRowData        = read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE);          #read data from y_test.txt

    # Nameing Columns
	colnames(activityTypeRowData)  = c('activityId','activityType');
	colnames(subjectTrainRowData)  = "subjectId";
	colnames(xTrainRowData)        = featuresRowData[,2]; 
	colnames(yTrainRowData)        = "activityId";
	colnames(subjectTestRowData)   = "subjectId";
	colnames(xTestRowData)         = featuresRowData[,2]; 
	colnames(yTestRowData)         = "activityId";

	# merging y_train.txt, subject_train.txt, and x_train.txt filedata
    trainingData = cbind(yTrainRowData,subjectTrainRowData,xTrainRowData);

	# merging y_train.txt, subject_train.txt, and x_train.txt file data
	testData = cbind(yTestRowData,subjectTestRowData,xTestRowData);

	# Combine training and test data by row binding
	mergedData = rbind(trainingData,testData);
		
	
	# -----------------------------------------------------------------------------
	# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
	# -----------------------------------------------------------------------------
	allColumnNamesOfmergedData = colnames(mergedData)
	
	wantedColumns = grep(".*activityId.*|.*subjectId.*|.*mean\\(\\).*|.*std\\(\\).*", allColumnNamesOfmergedData);
	
	meanStdData = mergedData[,wantedColumns];

	# -----------------------------------------------------------------------------
	# 3. Use descriptive activity names to name the activities in the data set
	# ----------------------------------------------------------------------------- 
	#meanStdData    = merge(activityTypeRowData,meanStdData,by='activityId',all.x=TRUE);
    #meanStdData$activityId = factor(meanStdData$activityId, levels = activityTypeRowData[,1], labels = activityTypeRowData[,2]);
	#meanStdData$subjectId <- as.factor(meanStdData$subjectId)
	# -----------------------------------------------------------------------------
	# 4. Appropriately label the data set with descriptive activity names. 
	# -----------------------------------------------------------------------------
	names(meanStdData) = gsub('activityId', 'Activity', names(meanStdData));
	names(meanStdData) = gsub('subjectId', 'Subject', names(meanStdData));
	names(meanStdData) = gsub('-mean', 'Mean', names(meanStdData));
	names(meanStdData) = gsub('-std', 'Std', names(meanStdData));
	names(meanStdData) = gsub('[-()]', '', names(meanStdData));
	
	
	# -----------------------------------------------------------------------------
	# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
	# -----------------------------------------------------------------------------
	tidyData.melted = melt(meanStdData, id = c("Subject", "Activity"));
	tidyData.mean = dcast(tidyData.melted, Subject + Activity ~ variable, mean);
	write.table(tidyData.mean, './tidyData.txt',row.names=TRUE,sep='\t');
  
 #restore back working directory.
 setwd(WORKING_DIR);

}

setwd("E:\\learning\\codebase\\week4")
UCI_HAR_data_analysis("Debug")# -----------------------------------------------------------------------------
#
# UCI HAR Dataset Analysis
#  Autor : Shibu Kumar Nair
# ref: https://github.com/hglennrock/getting-cleaning-data-project/blob/master/run_analysis.R
# ref:https://github.com/OscarPDR/Coursera-Getting-and-Cleaning-Data-Course-Project/blob/master/run_analysis.R
# -----------------------------------------------------------------------------
library(reshape2)

download_data <- function(downloadUrl,fileName) {
    
	os <- tolower(.Platform$OS.type)
	
	if(!file.exists(getwd())){
		stop(paste(getwd() ," is not a valid directory"))
	}
	
	if(os=="windows"){
		download.file(downloadUrl, paste("./",fileName))
	}else{
		download.file(downloadUrl, paste("./",fileName), method="curl")
	}
	
	if(grepl("^.*(.gz|.bz2|.tar|.zip|.tgz|.gzip|.7z)[[:space:]]*$", fileName)){
	   print(getwd())
		unzip(paste("./",fileName)) 
	 }
	
}

# -----------------------------------------------------------------------------
# Description:
#
# This function will perform the following steps on the UCI HAR Dataset 
# by downloaded from :-
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#      1. Merges the training and the test sets to create one data set.
#      2. Extracts only the measurements on the mean and standard deviation 
#         for each measurement.
#      3. Uses descriptive activity names to name the activities in the data 
#          set.
#      4. Appropriately labels the data set with descriptive variable names.
#      5. From the data set in step 4, creates a second, independent tidy data 
#         set with the average of each variable for each activity and each 
#         subject.
#
# Usage:
#
#     UCI_HAR_data_analysis()
#     
# Examples:
#
#     UCI_HAR_data_analysis()
#
# -----------------------------------------------------------------------------
UCI_HAR_data_analysis <- function() {

  rm(list = ls())
	
  WORKING_DIR =getwd();
  DOENLOAD_URL ="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  DATA_FILE_NAME = "UCI_HAR_dataset.zip"
  DATA_DIR ="./UCI HAR Dataset"
  
   download_data(DOENLOAD_URL,DATA_FILE_NAME)
  
   # -----------------------------------------------------------------------------
   # 1. Merges the training and the test sets to create one data set.
   # -----------------------------------------------------------------------------

   # Read in the data from files
    featuresRowData     = read.table('./UCI HAR Dataset/features.txt',header=FALSE);             #read data from  features.txt
    activityTypeRowData = read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE);      #read data from activity_labels.txt
    subjectTrainRowData = read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE);  #read data from subject_train.txt
    xTrainRowData       = read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE);        #read data from x_train.txt
    yTrainRowData       = read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE);        #read data from y_train.txt
	subjectTestRowData  = read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE);    #read data from subject_test.txt
	xTestRowData        = read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE);          #read data from x_test.txt
	yTestRowData        = read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE);          #read data from y_test.txt

    # Naming Columns
	colnames(activityTypeRowData)  = c('activityId','activityType');
	colnames(subjectTrainRowData)  = "subjectId";
	colnames(xTrainRowData)        = featuresRowData[,2]; 
	colnames(yTrainRowData)        = "activityId";
	colnames(subjectTestRowData)   = "subjectId";
	colnames(xTestRowData)         = featuresRowData[,2]; 
	colnames(yTestRowData)         = "activityId";

	# merging y_train.txt, subject_train.txt, and x_train.txt filedata
    trainingData = cbind(yTrainRowData,subjectTrainRowData,xTrainRowData);

	# merging y_train.txt, subject_train.txt, and x_train.txt file data
	testData = cbind(yTestRowData,subjectTestRowData,xTestRowData);

	# Combine training and test data by row binding
	mergedData = rbind(trainingData,testData);
	
	# -----------------------------------------------------------------------------
	# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
	# -----------------------------------------------------------------------------
	allColumnNamesOfmergedData = colnames(mergedData)
	
	wantedColumns = grep(".*activityId.*|.*subjectId.*|.*mean\\(\\).*|.*std\\(\\).*", allColumnNamesOfmergedData);
	
	meanStdData = mergedData[,wantedColumns];
	# -----------------------------------------------------------------------------
	# 3. Use descriptive activity names to name the activities in the data set
	# ----------------------------------------------------------------------------- 
	meanStdData$activityId = activityTypeRowData$activityType[match(meanStdData$activityId,activityTypeRowData$activityId)];
	
	head(meanStdData);

	# -----------------------------------------------------------------------------
	# 4. Appropriately label the data set with descriptive activity names. 
	# -----------------------------------------------------------------------------
	names(meanStdData) = gsub('activityId', 'Activity', names(meanStdData));
	names(meanStdData) = gsub('subjectId', 'Subject', names(meanStdData));
	names(meanStdData) = gsub('-mean', 'Mean', names(meanStdData));
	names(meanStdData) = gsub('-std', 'Std', names(meanStdData));
	names(meanStdData) = gsub('[-()]', '', names(meanStdData));
	
	# -----------------------------------------------------------------------------
	# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
	# -----------------------------------------------------------------------------
	tidyData.melted = melt(meanStdData, id = c("Subject", "Activity"));
	tidyData.mean = dcast(tidyData.melted, Subject + Activity ~ variable, mean);
	write.table(meanStdData, './tidy_data.txt',row.names = FALSE, quote = FALSE,sep='\t');
  
 #restore back working directory.
 setwd(WORKING_DIR);

}

setwd("E:\\learning\\codebase\\week4")
UCI_HAR_data_analysis()