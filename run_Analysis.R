library(data.table)

##read the file from working directory
fileurl = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset'


## read in the files from UCI HAR Dataset
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")  


# Merge the train and test sets to create one dataset
mergedDataset <- rbind(x_train,x_test)


# Extract only the measurements on the mean and standard deviation for each measurement 
## Create a vector for mean and std; use grep() for pattern matching and replacement
mean_std <- grep("mean()|std()", features[, 2]) 
mergedDataset <- mergedDataset[,mean_std]


# Appropriately label the data set with descriptive variable names.
## Create vector to simplify results using sapply to call gsub function to replace original activity names with descriptive names
featureNames <- sapply(features[, 2], function(x) {gsub("[()]", "",x)})
names(mergedDataset) <- featureNames[mean_std]

## Combine subject data and activity data with mergedDataset
subject <- rbind(subject_train, subject_test)
names(subject) <- 'subject'
activity <- rbind(y_train, y_test)
names(activity) <- 'activity'
mergedDataset <- cbind(subject,activity, mergedDataset)


# Use descriptive activity names
## group the activity column of mergedDataset; create factor levels based on the activity_labels
activity_names <- factor(mergedDataset$activity)
levels(activity_names) <- activity_labels[,2]
mergedDataset$activity <- activity_names


# Creates a second, independent tidy data set with the average of each variable for each activity and each subject 
## reshape2 package to transform the data into a tidy dataset
library("reshape2")

## melt() to identify the unique ID variables
dataSet <- melt(mergedDataset,(id.vars=c('subject','activity')))

## dcast () to resphape and aggregate the data
tidyDataSet <- dcast(dataSet, subject + activity ~ variable, mean)

## write the new tidy data to the working directory
write.table(tidyDataSet, "tidy_UCI_HAR_Dataset.txt", sep = ",", row.name = FALSE)
