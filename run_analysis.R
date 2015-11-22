##run_analysis.R
##This script is for the "Getting and Cleaning Data" Course Project
##This script outputs a dataset "tidy_data.csv"

##This script does the following:
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names. 
# 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

if (!file.exists("./data")) {
        dir.create("./data")
}


fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./data/data_zip.zip", method="curl")

##Load Test Data
test_x_data <- read.table("./data/UCI HAR Dataset/test/X_test.txt", sep = "", header = F, na.strings ="", stringsAsFactors= F)
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
test_y_data <- read.table("./data/UCI HAR Dataset/test/y_test.txt")

##Load Training Data
train_x_data <- read.table("./data/UCI HAR Dataset/train/X_train.txt", sep = "", header = F, na.strings ="", stringsAsFactors= F)
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
train_y_data <- read.table("./data/UCI HAR Dataset/train/y_train.txt")

##Load Column Names
features <- read.table("./data/UCI HAR Dataset/features.txt")

##Load dplyr
library(plyr)
library(dplyr)

##Name columns 
colnames(test_x_data) <- features[,2]
colnames(train_x_data) <- features[,2]

##Create new data frame that will rename duplicate variable names
test_x_data <- test_x_data[,]
train_x_data <- train_x_data[,]


##Subset variables to only those that are means and standard deviations
##Subset data based on name of Varaible--returnds varaibles "train_x_subset2" and "test_x_subset2" with 79 variables)
train_x_subset <- select(train_x_data, matches("mean()|std()"))
test_x_subset <- select(test_x_data, matches("mean()|std()"))
train_x_subset2 <- select(train_x_subset, -starts_with("angle"))
test_x_subset2 <- select(test_x_subset, -starts_with("angle"))



##Merge subject and test into data
test_merge <- cbind(subject_test[,1],test_y_data[,1], test_x_subset2)
train_merge <- cbind(subject_train[,1], train_y_data[,1], train_x_subset2)



##Rename Subject and Activity Test Columns
#test_merge <- rename(test_merge, c("test_y_data[, 1]"="Activity_Test", "subject_test[, 1]"="Subject"))
names (test_merge)[1] <- "Subject"
names (test_merge)[2] <- "Activity_Test"
##train_merge <- rename(train_merge, c("train_y_data[, 1]"="Activity_Test", "subject_train[, 1]"="Subject"))
names (train_merge)[1] <- "Subject"
names (train_merge)[2] <- "Activity_Test"



## Merge Test and Train Datasets
merged_all <- rbind(train_merge, test_merge)



##Turn Activity Test into factor with names into new data farme "merged_all2"
merged_all$Activity_Test <- factor(merged_all$Activity_Test, labels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))


##Group by Subject
subject_group <- group_by(merged_all, Subject, Activity_Test)
temp_set <- select(subject_group, -(Subject:Activity_Test))
tidy_data <- summarise_each(temp_set, funs(mean))

##Write dataset to a file
write.table(tidy_data, file="tidy_data.txt", row.names = FALSE)
