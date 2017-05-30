## Getting and Cleaning Data - Assignment 4
## by DJD
## 05/25/2017

# Set working directory
#====== >setwd("yourWORKINGDIRCTORYhere")

# Loading libraries needed to "melt" and "arrange" table and columns respectively
library (reshape2)
library (dplyr)
         
# Create work folder to extract and write data to
if(!file.exists("./Assignment4")){dir.create("./Assignment4")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# Download raw data files
download.file(fileUrl,destfile="./assignment4/Dataset.zip")

# Unzip raw dataSet to /Assignment4 directory
unzip(zipfile="./Assignment4/Dataset.zip",exdir="./Assignment4")

# Read the raw data text files for "test", "train", "features", & "activity lables" from 
# their original folder positions as received during unzip
# Reading txt test tables
txt_test_x <- read.table("./Assignment4/UCI HAR Dataset/test/X_test.txt")
txt_test_y <- read.table("./Assignment4/UCI HAR Dataset/test/y_test.txt")
txt_test_subject <- read.table("./Assignment4/UCI HAR Dataset/test/subject_test.txt")

# Reading txt trainings tables
txt_train_x <- read.table("./Assignment4/UCI HAR Dataset/train/X_train.txt")
txt_train_y <- read.table("./Assignment4/UCI HAR Dataset/train/y_train.txt")
txt_train_subject <- read.table("./Assignment4/UCI HAR Dataset/train/subject_train.txt")

# Reading "features" and  "activity label" information tables
txt_features <- read.table("./Assignment4/UCI HAR Dataset/features.txt")
txt_lables <- read.table("./Assignment4/UCI HAR Dataset/activity_labels.txt")

# Assign column names for test and training data
colnames(txt_test_x) <- txt_features[,2]     
colnames(txt_test_y) <- "activityId"        
colnames(txt_test_subject) <- "subjectId"  

colnames(txt_train_x) <- txt_features[,2]    
colnames(txt_train_y) <- "activityId"       
colnames(txt_train_subject) <- "subjectId"  

colnames(txt_lables) <- c("activityId", "activityType") 

# Merging column named txt files from above into single data set
# Merge test and train data with column order: "activityID, "subjectID", "subjectID"
test_merge <- cbind( txt_test_subject, txt_test_y,txt_test_x)
train_merge <- cbind( txt_train_subject,txt_train_y, txt_train_x)

# Merge test and train data into one data set
combinedTable <- rbind(test_merge, train_merge)

# Extracting columns with "mean" & "standard deviation" data
mean_value <- grep ( "mean", colnames(combinedTable), ignore.case = TRUE )
stddev_value <- grep ( "std", colnames(combinedTable), ignore.case = TRUE)

# Combine the mean and standard deviation data into one table
combinedDataSet <- combinedTable[ c ( 1:3, mean_value, stddev_value ) ]

# Reshape the dataset, using the "melt fucntion" from the "reshape2 library" 
# Basically the dataset is "melt" so that each row is a unique id-variable combination. 
# Then the "melt" dataset is "cast" into the shape needed.
# First the melt:
mean_stddev_labeled <- melt(combinedDataSet, id = c ("subjectId", "activityId" ), measure.vars = c(4:89))

# Second the cast, extracting "only the 'mean data', i.e. "averages for each variable, activity, and subject":
average_mean_labeled <- dcast (mean_stddev_labeled, subjectId + activityId ~ variable, mean)

# Create a clean / tidy data set by adding the "activity" label to the data set from "txt_ lables" file
average_mean_labeled <- merge(txt_lables, average_mean_labeled, by="activityId")
# Reorder the columns in the sequence "subjectId", activityType", "variable", & "value"
average_mean_labeled <- average_mean_labeled[ , c(3,2,1,4:89)]
# Rename "variable" column to "exerciseVariable"
colnames(average_mean_labeled)[4]  <- "exerciseValue"

# Order the data first by "subjectId" and then "activityId"
average_mean_labeled <- arrange(average_mean_labeled,(average_mean_labeled$subjectId))
average_mean_labeled <- arrange(average_mean_labeled,(average_mean_labeled$activityId))

# Write a copy of the  "average_mean_labeled" data tabel to .txt file in an "/Assignment4/Results" folder
if(!file.exists("./Assignment4/Results")){dir.create("./Assignment4/Results")}
write.table(average_mean_labeled,"./Assignment4/Results/average_mean_labeled.txt",row.names=FALSE)

















