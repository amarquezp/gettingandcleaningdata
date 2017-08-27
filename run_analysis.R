library(dplyr)
library(data.table)
library(plyr)

mergeTrainAndTest <- function(){
	
	# Define the path to the different file paths
	
	FEATURES_FILEPATH		<- "UCI HAR Dataset/features.txt"
	ACTIVITIES_FILEPATH		<- "UCI HAR Dataset/activity_labels.txt"
	
	TRAIN_DATA_FILEPATH		<- "UCI HAR Dataset/train/X_train.txt"
	SUBJECT_TRAIN_FILEPATH	<- "UCI HAR Dataset/train/subject_train.txt"
	ACTIVITIES_TRAIN_FILEPATH<- "UCI HAR Dataset/train/y_train.txt"
	
	TEST_DATA_FILEPATH		<- "UCI HAR Dataset/test/X_test.txt"
	SUBJET_TEST_FILEPATH	<- "UCI HAR Dataset/test/subject_test.txt"
	ACTIVITIES_TEST_FILEPATH	<- "UCI HAR Dataset/test/y_test.txt"
	
	
	# Read the files to get the data into tables
	# FeaturesDT will be used to determine the name of the columns for X_Train and X_Test
	
	featuresDT 	<- read.table(FEATURES_FILEPATH, header = FALSE)
	
	# activitiesDT will be used to determine the name activities column added to X_Train and X_Test
	activitiesDT		<- read.table(ACTIVITIES_FILEPATH, header = FALSE)
	activitiesTrainDT	<- read.table(ACTIVITIES_TRAIN_FILEPATH, header = FALSE)
	activitiesTestDT	<- read.table(ACTIVITIES_TEST_FILEPATH, header = FALSE)
	subjectsTrainDT	<- read.table(SUBJECT_TRAIN_FILEPATH)
	subjectsTestDT		<- read.table(SUBJET_TEST_FILEPATH)
	
	# set features data table descriptive variable names
	names(featuresDT)		<- c("feature_id", "feature_name")
	names(activitiesDT) 	<- c("activity_id", "activity_name")
	names(activitiesTrainDT) <- c("activityTrain_id")
	names(activitiesTestDT)	<- c("activityTest_id")
	names(subjectsTrainDT)	<- c("subject_id")
	names(subjectsTestDT)	<- c("subject_id")
	
	# Will use the features to set variable namaes for TRAIN and TEST data sets, so cleaning commas and parenthesis
	# Since there are repeated feature names/values, I use the index of each feature to concatenate with the name
	# so that way every variable name is unique
	featuresDT$feature_name <- tolower(paste(gsub("[(|),]", featuresDT$feature_name, rep=""), "-", featuresDT$feature_id, sep=""))
	
	# Create a new column that will hold the descriptive variable for training acts
	# Then assign the descriptive name from the ACTIVITY labels based on the ID
	activitiesTrainDT$activityName <- ""
	activitiesTrainDT$activityName <- activitiesDT[activitiesTrainDT$activityTrain_id, 2]
	
	# Create a new column that will hold the descriptive variable for training acts
	# Then assign the descriptive name from the ACTIVITY labels based on the ID
	activitiesTestDT$activityName <- ""
	activitiesTestDT$activityName <- activitiesDT[activitiesTestDT$activityTest_id, 2]
	
	
	# Prepare the training data set:
	# Read raw data from file
	# Create and initialize the activity variable which will identify every sample with an activity
	xtrainDT				<- read.table(TRAIN_DATA_FILEPATH, header = FALSE)
	xtrainDT$activityName 	<- ""
	xtrainDT$activityName	<- activitiesTrainDT$activityName
	xtrainDT$subjectId		<- ""
	xtrainDT$subjectId		<- subjectsTrainDT$subject_id
	# reorder the data set to have subject id and activity name as the first 2 columns
	xtrainDT 				<- xtrainDT[, c(ncol(xtrainDT),ncol(xtrainDT)-1 , 1:(ncol(xtrainDT)-2))]
	xtrainDT$activityName	<- sapply(xtrainDT[,"activityName"], as.character)
	
	# Prepare the test data set:
	# Read raw data from file
	# Create and initialize the activity variable which will identify every sample with an activity
	xtestDT				<- read.table(TEST_DATA_FILEPATH, header = FALSE)
	xtestDT$activityName 	<- ""
	xtestDT$activityName	<- activitiesTestDT$activityName
	xtestDT$subjectId		<- ""
	xtestDT$subjectId		<- subjectsTestDT$subject_id
	# reorder the data set to have subject id and activity name as the first 2 columns
	xtestDT 				<- xtestDT[, c(ncol(xtestDT), ncol(xtestDT)-1, 1:(ncol(xtestDT)-2))]
	xtestDT$activityName	<- sapply(xtestDT[,"activityName"], as.character)
	
	# Merge train and test data sets, using the intersect of the columns to have all columns considered and
	# no new one created, the resulting of the merge should be a data set with the following: 
	# number of rows: nrow(train) + nrow(test)
	# variables: the same variables for both, described in the features data set, plus the activity variable
	mergedTrainTestDT			<- merge(xtrainDT, xtestDT, by = intersect(names(xtrainDT), names(xtestDT)), all=TRUE)
	names(mergedTrainTestDT)[3:ncol(mergedTrainTestDT)] <- featuresDT$feature_name
	
	# get the mean and std columns, searching for those columns that represent a "std":
	finalDT <- mergedTrainTestDT[,names(mergedTrainTestDT) %in% grep("subjectId|activityName|*.mean.*|*.std.*", names(mergedTrainTestDT), ignore.case=TRUE, value=TRUE)]
	
	tidydataset <- createTidyDataSetFromMerged(finalDT)
	
	# write the data table to a TXT file, omit the row names
	write.table(tidydataset, file="tidydataset.txt", quote = FALSE, row.names = FALSE) 
	# return the data table for further processing, in my case, I separated the merging of TRAIN and TEST data tables from 
	# the tidy data set with only mean values
	tidydataset
}

# This function extracts a tidy data set and calculates the average of each variable for each activity and each subject.
createTidyDataSetFromMerged <- function(aDataTable){
	nsubjects		<- nrow(distinct(aDataTable, subjectId))
	nactivities	<- nrow(distinct(aDataTable, activityName))
	activityLevels <- distinct(aDataTable, activityName)
	
	
	# change from factors to characters, will be useful when comparing activity names 
	activityLevels$activityName <- sapply(activityLevels[,"activityName"], as.character)
	
	tidydataset	<- data.table(subject_id=numeric(), activityName=character()) 
	
	for( i in 1:nsubjects ){
		
		for ( j in 1:nactivities ){
			
			tidydataset <- rbind(tidydataset, data.table(subject_id=i,activityName=activityLevels[j,] ), fill=TRUE)
			
			for( k in 3:ncol(aDataTable) ){

				# calculate the mean
				meanBy_k <- mean(aDataTable[which(aDataTable$subjectId==i & aDataTable$activityName==activityLevels[j,]) , k])
				# important to note how the row is set here, with the "which" function, so that way the pointer inside the data set
				# is not pointing to a different position
				tidydataset[ which(tidydataset$subject_id==i & tidydataset$activityName==activityLevels[j,]) ,paste("mean-",names(aDataTable)[k], sep="") := meanBy_k]
			}

		}

	}
	
	tidydataset
	
}