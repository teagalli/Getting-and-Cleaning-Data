
#install.packages("plyr")
library("plyr")

projectFolder <- "~/Getting and Cleaning Data/Week 5/Project" # file folder location
dir.create(projectFolder)
setwd(projectFolder)

dataPath <- paste0(projectFolder,"/UCI-HAR-Dataset.zip")
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(url, dataPath)
unzip(dataPath, list = FALSE, overwrite = TRUE, exdir = paste0(projectFolder))

dataFolder <- paste0(projectFolder,"/UCI HAR Dataset")

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 0. Merges the training and the test sets to create one data set (testing)

# get list of all 561 features
featuresList <- read.table(paste0(dataFolder,"/features.txt"), header = FALSE)

# get train group data

# get train group subjects
trainSubject <- read.table(paste0(dataFolder,"/train/subject_train.txt"), header = FALSE)
names(trainSubject) <- c("Subject")

# get train group activities 
trainActivity <- read.table(paste0(dataFolder,"/train/y_train.txt"), header = FALSE)
names(trainActivity) <- c("Activity")

# get train group features
trainFeatures <- read.table(paste0(dataFolder,"/train/X_train.txt"), header = FALSE)
names(trainFeatures) <- featuresList$V2

  ## verify train group data
  nrow(trainSubject) == nrow(trainActivity)
  nrow(trainActivity) == nrow(trainFeatures)
  ncol(trainFeatures) == 561
  nrow(unique(trainActivity)) == 6

# aggregate train group dataset
trainDataSet <- cbind(trainSubject, trainActivity, trainFeatures)

# add train group flag
trainDataSet$GroupFlag <- "TrainGroup"

  ## verify aggregate train group data
  nrow(trainDataSet) == nrow(trainFeatures)
  ncol(trainDataSet) == ncol(trainFeatures) + ncol(trainActivity) + ncol(trainSubject) + 1

  
# get test group data set

# get test group subject data
testSubject <- read.table(paste0(dataFolder,"/test/subject_test.txt"), header = FALSE)
names(testSubject) <- c("Subject")

# get test group activity data
testActivity <- read.table(paste0(dataFolder,"/test/y_test.txt"), header = FALSE)
names(testActivity) <- c("Activity")

# get test group feature data
testFeatures <- read.table(paste0(dataFolder,"/test/X_test.txt"), header = FALSE)
names(testFeatures) <- featuresList$V2

  ## verify test group data
  nrow(testSubject) == nrow(testActivity)
  nrow(testActivity) == nrow(testFeatures)
  ncol(testFeatures) == 561
  nrow(unique(testActivity)) == 6

# aggregate test group dataset
testDataSet <- cbind(testSubject, testActivity, testFeatures)
testDataSet$GroupFlag <- "TestGroup"

  ## verify aggregate test group data
  nrow(testDataSet) == nrow(testFeatures)
  ncol(testDataSet) == ncol(testFeatures) + ncol(testActivity) + ncol(testSubject) + 1
  
  # verify train and test group data
  names(trainDataSet) == names(testDataSet)

  
# verify train and test group data
nrow(unique(rbind(testSubject, trainSubject))) == nrow(unique(testSubject)) + nrow(unique(trainSubject))
nrow(unique(rbind(testSubject, trainSubject))) == 30    

# merge train and test datasets
aggregateData <- rbind(trainDataSet, testDataSet)

head(aggregateData)
nrow(aggregateData)
ncol(aggregateData)
names(aggregateData)
sum(unique(aggregateData$Subject)) == sum(1:30)
sum(unique(aggregateData$Activity)) == sum(1:6)

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1. Merges the training and the test sets to create one data set 
# by creating a function to retrieve group activity, subject and feature variables data

# get group data function
# the function uses the featuresList which was imported above and the 
# dataFolder path defined above as default variables

getGroupData <- function(group, features = featuresList, folder = dataFolder) {
  
  # check given group is valid
  if(!group %in% c("train", "test")){
    print("check group name")
  } else{
  # get group subjects list
  subjectData = read.table(paste0(folder,"/",group,"/subject_",group,".txt"), header = FALSE)
  names(subjectData) = c("Subject")
  
  # get group activities  list
  activityData = read.table(paste0(folder,"/",group,"/y_",group,".txt"), header = FALSE)
  names(activityData) = c("Activity")
  
  # get group features data table 
  featuresData = read.table(paste0(folder,"/",group,"/X_",group,".txt"), header = FALSE)
  
  # assign column names to group features data
  names(featuresData) = features$V2
  
  # aggregate group dataset
  groupDataSet = cbind(subjectData, activityData, featuresData)
  
  # add group flag
  groupDataSet$GroupFlag <- paste0(group,"Group")
  return(groupDataSet)
  }
}

trainGroupData <- getGroupData("train")
testGroupData <- getGroupData("test")

# merge train and test datasets
aggregateGroupData <- rbind(trainGroupData, testGroupData)
aggregateGroupData

  # sanity checks
  head(aggregateGroupData) 
  nrow(aggregateGroupData) # number of rows is equal to the number of observations (10299 in total)
  ncol(aggregateGroupData) # number of columns is equal to the 561 features, the subject, the activity and the group flag (564 in total)
  names(aggregateGroupData) # check column names correspond to the correct variable
  sum(unique(aggregateGroupData$Subject)) == sum(1:30) # check that there are observations for all 30 subjects
  sum(unique(aggregateGroupData$Activity)) == sum(1:6) # check that there are observations for all 6 activities performed


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

subsetFeatureLabels <- subset(featuresList$V2, grepl('(mean\\(\\)|std\\(\\)|meanFreq\\(\\))', featuresList$V2, ignore.case=T) == TRUE)
subsetFeatureData <- aggregateGroupData[, names(aggregateGroupData) %in% subsetFeatureLabels]
subsetFeatureData

# 3. Uses descriptive activity names to name the activities in the data set

activityLables <- read.table(paste0(dataFolder,"/activity_labels.txt"), header = FALSE)
names(activityLables) <- c("Activity", "ActivityLabel")
aggDataWithActivityLabels <- join(activityLables, aggregateGroupData)
aggDataWithActivityLabels

# 4. Appropriately labels the data set with descriptive variable names.
# see above  

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
# for each activity and each subject.

avgBySubjectActivity <- aggregate(aggDataWithActivityLabels[,!names(aggDataWithActivityLabels) %in% c("Activity", "ActivityLabel", "Subject", "GroupFlag")], 
          by = list(aggDataWithActivityLabels$ActivityLabel, aggDataWithActivityLabels$Subject),
          FUN = mean)
names(avgBySubjectActivity[,1:2]) <- c("Activity", "Subject")
avgBySubjectActivity

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Project Review Criteria:

# 1. Data set is tidy

# Yes, the all above data sets satify the following three rules for tidy data:
  # a. Each variable in the data set is placed in its own column
  # b. Each observation is placed in its own row
  # c. Each value is placed in its own cell

# 2. Github repo contains the required scripts
  # Yes, the github repo for this assingment contains the required script: run_analysis.R

# 3.GitHub contains a code book that modifies and updates the available codebooks 
# with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information. 
  # Yes, there is a codebook.md included which defines and summarizes calculations, variables, etc.

# 4. The README that explains the analysis files is clear and understandable.
  # Yes, there is a clear and understandable README file which explains the analysis

# 5. The work submitted for this project is the work of the student who submitted it.
  # Yes, the work submitted for this project was done by the student who submitted it

