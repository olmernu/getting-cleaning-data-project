########################################################
##
## Getting and cleanning data
## Project
## Olmer Nunez-Sosa
## 2016-02-28
##
########################################################

################################################################
# 1. Merge the training and the test sets to create one data set.

#set working directory to the location where the UCI HAR Dataset was unzipped
setwd("./UCI HAR Dataset")

#Description from: 
#http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.names

# Read in the data from files
features     = read.table('./features.txt',header=FALSE)
activityType = read.table('./activity_labels.txt',header=FALSE)
subjectTrain = read.table('./train/subject_train.txt',header=FALSE)
xTrain       = read.table('./train/x_train.txt',header=FALSE)
yTrain       = read.table('./train/y_train.txt',header=FALSE)

# Assigin column names to the data imported above
head(features)
  ##List of all variables

head(activityType)
colnames(activityType) = c("activityId","activityType")
  ##List of activities  

head(subjectTrain)
tail(subjectTrain)
colnames(subjectTrain) = "subjectId"
  ##Each row identifies the subject who performed the activity for 
  ##each window sample. Its range is from 1 to 30.

head(xTrain)
colnames(xTrain) = features[,2]
  ##Training set

head(yTrain); tail(yTrain)
colnames(yTrain) = "activityId"
  ##Training labels

# Create the final training set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain)
head(trainingData)
table(trainingData$activityId)
table(trainingData$subjectId)
table(trainingData$activityId, trainingData$subjectId)

# Read in the test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE)
xTest       = read.table('./test/x_test.txt',header=FALSE)
yTest       = read.table('./test/y_test.txt',header=FALSE)


# Assign column names to the test data imported above
head(subjectTest)
colnames(subjectTest) = "subjectId"
  ##Each row identifies the subject who performed the activity for 
  ##each window sample. Its range is from 1 to 30.

head(xTest)
colnames(xTest)      = features[,2]
  ##Test set

head(yTest)
colnames(yTest)       = "activityId"
  ##Test labels

# Create the final test set by merging the xTest, yTest and subjectTest data
testData = cbind(yTest,subjectTest,xTest)
View(testData)
head(testData)
table(testData$activityId)
table(testData$subjectId)
table(testData$activityId, testData$subjectId)

# Combine training and test data to create a final data set
identical(names(testData), names(trainingData))
finalData = rbind(trainingData,testData)

# Remove any unnecessary objects
rm(features, subjectTest, subjectTrain, xTest, xTrain, yTest, yTrain)
rm(trainingData, testData)


################################################################
# 2. Extract only the measurements on the mean and standard deviation 
#    for each measurement. 

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames  = colnames(finalData)
colNames

# Create a logicalVector that contains TRUE values for the ID, 
# mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activityId",colNames) | grepl("subjectId",colNames) | 
                   grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) 
                 & !grepl("mean..-",colNames) |
                   grepl("-std..",colNames) & !grepl("-std()..-",colNames))

table(logicalVector)

# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector==TRUE]
View(finalData)

#Remove unneccesary objects
rm(colNames, logicalVector)


################################################################
# 3. Use descriptive activity names to name the activities in the data set

#load activityType
View(activityType)
intersect(names(activityType), names(finalData))

# Merge the finalData set with the acitivityType table to include 
# descriptive activity names
finalData = merge(activityType,finalData, by='activityId',all.x=TRUE)


################################################################
# 4. Appropriately label the data set with descriptive activity names. 

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData)
colNames

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames
names(finalData)

# Remove unnecessary objects
rm(colNames, i)


################################################################
# 5. Create a second, independent tidy data set with the average of each 
#    variable for each activity and each subject. 

tidyData = aggregate(finalData[,names(finalData) 
                                     != c('activityId','activityType','subjectId')],
                     by=list(activityId=finalData$activityId,
                             activityType=finalData$activityType,
                             subjectId =finalData$subjectId),
                     mean)

# Export the tidyData set 
write.table(tidyData, "./tidyData.txt",row.names=TRUE,sep='\t')

