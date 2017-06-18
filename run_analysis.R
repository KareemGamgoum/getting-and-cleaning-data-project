#Peer-graded Assignment: Getting and Cleaning Data Course Project

#By Kareem Gamgoum

#The purpose of this project is to demonstrate your ability to collect, work with,
# and clean a data set.

#You should create one R script called run_analysis.R that does the following.

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with
#    the average of each variable for each activity and each subject.

###################################

# Clear the workspace
rm(list=ls())

# 1. Merge the training and the test sets to create one data set.


        # set working directory
        setwd('/Users/kareem.gamgoum/Desktop/DataScience/Course 3 - Getting and Cleaning Data/Course Project/UCI HAR Dataset/')

        # read in the train data 
        
        x_train <- read.table("./train/X_train.txt")
        y_train <- read.table("./train/y_train.txt")
        subject_train <- read.table("./train/subject_train.txt")
        
        # read in the test data 
        
        x_test <- read.table("./test/X_test.txt")
        y_test <- read.table("./test/y_test.txt")
        subject_test <- read.table("./test/subject_test.txt")      
        
        # read in the miscellaneous data
        
        features <- read.table('./features.txt')
        activity_labels <- read.table('./activity_labels.txt')
        
        # Assigin column names to the data
        
        colnames(activity_labels) <- c('activityId','activityType')
        colnames(subject_train) <- "subjectId"
        colnames(x_train) <- features[,2]
        colnames(y_train) <- "activityId"
        colnames(subject_test) <- "subjectId"
        colnames(x_test) <- features[,2]
        colnames(y_test) <- "activityId"

        # merge the data to get all in one table
        
        trainData <- cbind(y_train,subject_train,x_train)
        testData <- cbind(y_test,subject_test,x_test)
        
        allData <- rbind(trainData,testData)
        
# 2. Extract only the measurements on the mean and standard deviation for each measurement.
        
        # create a vector to read the column names
        
        colNames <- colnames(allData)
        
        # create a logical vector to bring back just the mean and standard deviation 
        
        mean_and_sd <- (grepl("activityId" , colNames) | 
                                 grepl("subjectId" , colNames) | 
                                 grepl("mean.." , colNames) | 
                                 grepl("std.." , colNames)
                                 ) & !grepl("meanFreq..", colNames)
        
        # subset allData table to only contain mean and standard deviation measures
        
        allData <- allData[mean_and_sd==TRUE]
        
# 3. Use descriptive activity names to name the activities in the data set
        
        # Merging the allData set with the activity_labels table to
        # include descriptive activity names
        
        allData <- merge(allData,activity_labels,by='activityId',all.x=TRUE)
        
        # Update the colNames vector to refresh after we merged
        colNames <- colnames(allData)

# 4. Appropriately labels the data set with descriptive variable names.
        
        # Rename the variables to be clearer and more meaningfull
        
        for (i in 1:length(colNames)) {
                colNames[i] <- gsub("\\()","",colNames[i])
                colNames[i] <- gsub("-std$","StandardDeviation",colNames[i])
                colNames[i] <- gsub("-mean","Mean",colNames[i])
                colNames[i] <- gsub("^(t)","Time",colNames[i])
                colNames[i] <- gsub("^(f)","Freq",colNames[i])
        }

        # Update the colNames vector to refresh after we have renamed

        colnames(allData) <- colNames
        
# 5. From the data set in step 4, creates a second, independent tidy data set with
# the average of each variable for each activity and each subject.
        
        # Creating a second table removing the activityType column
        allData2 <- allData[,names(allData) != 'activityType']
        
        # aggregating the table to only include the mean of each variable
        # for each activity and each subject
        allData2 <- aggregate(allData2[,names(allData2) != c('activityId','subjectId')],
        by=list(activityId=allData2$activityId,subjectId = allData2$subjectId),mean)

        # Merging the allData2 set with activityType to include descriptive
        # activity names
        allData2 <- merge(allData2,activity_labels,by='activityId',all.x=TRUE)
        allData2 <- allData2[order(allData2$subjectId, allData2$activityId),]
        
        # Export the tidyData set 
        write.table(allData2, './TidyDataSet.txt',row.names=TRUE,sep='\t')
        
                
