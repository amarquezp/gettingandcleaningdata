Peer-graded Assignment: Getting and Cleaning Data Course Project

# run_analysis.R
# Prerrequisites:
1. R installed :)
2. The raw data exists in a folder UCI HAR Dataset, which is directly from unzipping the downloaded package from URL: 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# To run the project:
1. source("run_analysis.R")
2. mergeTrainAndTest()

# Algorithm
1. Define the path to the different file paths
2.  Read the files to get the data into tables. 
  NOTE: Features will be used to determine the name of the columns for X_Train and X_Test. Will use lower case names, no parenthesis and            replacing commas with dashes -.
        Activities will be used to determine the name activities column added to X_Train and X_Test
3. Read the data sets, respectively: xTrainDT and xTestDT
4. Merge train and test data sets, using the intersect of the columns to have all columns considered and
5. Get the mean and std columns, searching for those columns including words like "mean" and "std". Also including key columns subjectId and activityName
6. Get the tidy data set, with the average of each variable for each activity and each subject.
7. Write the tidy data set
