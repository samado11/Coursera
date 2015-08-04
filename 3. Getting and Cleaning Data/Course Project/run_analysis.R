# Above all, Download the data files.
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "dataset.zip", method = "curl")
unzip("./dataset.zip", exdir = "./")

# Import Data files in created folder into R.
features <- read.table("./UCI HAR Dataset/features.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
train_x <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("./UCI HAR Dataset/train/y_train.txt")
test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
test_x <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("./UCI HAR Dataset/test/y_test.txt")

#Step 1. 
#combine data frame with test_df and train_df.
test_df <- cbind(test_subject, test_y, test_x)
train_df <- cbind(train_subject, train_y, train_x)

# Change Column names of combined data frame in above.
name.features <- as.vector(features[,2])
names(test_df) <- c("subject", "activity", name.features)
names(train_df) <- c("subject", "activity", name.features)
names(activity_labels) <- c("category", "activity")
dataset <- rbind(test_df, train_df)

#Step 2.
#Extract names of columns containing words "std", "mean" using 'grep' function.
extract_target <- c(grep("std", name.features), grep("mean", name.features))
#Exclude names of columns containing "meanFreq". Because I think that there are not the mean value of measurements.
extract_target <- extract_target[!(extract_target %in% c(grep("meanFreq", name.features)))]
#extract columns using column names I made( '+2' was used for skiping columns of 'activity', and 'subject')
extract_dataset <- dataset[ ,c(1:2, extract_target + 2)]


#Step 3.
#Mapping names of activities with data frame of 'activity_labels'
extract_dataset <- merge(activity_labels, extract_dataset, by.x = "category", by.y = "activity")
extract_dataset <- extract_dataset[, 2:69]

#Step 4.
#Change names of extract_dataset's columns to recognize easily
var.change <- names(extract_dataset)
var.change <- gsub("^t", "time_", var.change)
var.change <- gsub("^f", "freq_", var.change)
var.change <- gsub("Acc", "Accelerator", var.change)
var.change <- gsub("-std", "_sd", var.change)
var.change <- gsub("-mean", "_mean", var.change)
var.change <- gsub("Mag", "_Mag", var.change)
var.change <- gsub("Jerk", "_Jerk", var.change)
var.change <- gsub("BodyBody", "Body", var.change)
var.change <- gsub("[:():]", "", var.change)
var.change <- gsub("-Z$", "_z", var.change)
var.change <- gsub("-Y$", "_y", var.change)
var.change <- gsub("-X$", "_x", var.change)
var.change <- gsub("sd_x", "x_sd", var.change)
var.change <- gsub("sd_y", "y_sd", var.change)
var.change <- gsub("sd_z", "z_sd", var.change)
var.change <- gsub("mean_x", "x_mean", var.change)
var.change <- gsub("mean_y", "y_mean", var.change)
var.change <- gsub("mean_z", "z_mean", var.change)
names(extract_dataset) <- var.change

#Step 5.
#Load required packages
if(!require("tidyr")){install.packages("tidyr")}
if(!require("dplyr")){install.packages("dplyr")}

#Make mock-up of tidy_dataset we finally make to.
tidy_dataset <- extract_dataset[1, ]
#Extract data frames is consisted of a one activity and a one subject.
#And, Calculate mean value of every measurement columns.
#So, we shoud loop the calculting 180 times.
for(i in 1:6){
        for(j in 1:30){
                handle_data <- extract_dataset[extract_dataset$activity == 
                activity_labels[i, 2] & extract_dataset$subject == j,]
                
                tidy_dataset <- rbind(c(handle_data[1, 1:2], 
                apply(handle_data[,3:68], 2, mean)), tidy_dataset)
        }
}

#Make the tidy_datset more tidly.
tidy_dataset <- arrange(tidy_dataset, subject, activity)

#Export output into working directory
write.table(tidy_dataset, "./tidy_dataset.txt", row.names = FALSE)